#' Package version life duration at specific Date or for a specific version
#' @description using CRAN website to get a package life duration for certain version or at a specific Date.
#' @param pac character a package name.
#' @param version character version of package, by default the local version is taken if not available then the newest is assumed. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @param lib.loc character vector. Is omitted for non NULL version. Default: NULL
#' @param repos character the base URL of the repository to use. Default `biocran_repos()`
#' @return `difftime`, number of days package version was the newest one.
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' Results are cached for 1 hour with `memoise` package, memory cache.
#' @export
#' @examples
#' \dontrun{
#' pac_lifeduration("memoise")
#' pac_lifeduration("dplyr", version = "0.8.0")
#' pac_lifeduration("dplyr", at = as.Date("2019-02-14"))
#' }
pac_lifeduration <- function(pac,
                             version = NULL,
                             at = NULL,
                             lib.loc = NULL,
                             repos = biocran_repos()) {
  stopifnot(length(pac) == 1 && is.character(pac))
  stopifnot(!all(c(!is.null(version), !is.null(at))))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))

  is_installed <- isTRUE(pac %in% rownames(installed_packages(lib.loc = lib.loc)))

  if (!pac %in% rownames(available_packages(repos = repos)) && !is_installed) {
    return(NA)
  } else {
    last_version <- pac_last(pac, repos = repos)
  }


  if ((is.null(version) && is.null(at)) ||
    (!is.null(version) && isTRUE(utils::compareVersion(version, last_version) == 0))) {

    if (is_installed) {
    descr <- utils::packageDescription(pac, lib.loc = lib.loc)
    if (isTRUE(utils::compareVersion(descr[["Version"]], last_version) == 0)) {
      base_date <- descr[["Date/Publication"]]
      if (is.null(base_date)) base_date <- descr[["Date"]]
      life <- Sys.Date() - as.Date(as.character(substr(base_date, 1, 10)))
      if (identical(life, structure(numeric(0), class = "difftime", units = "days"))) life <- NA
      return(life)
    } else {
      life <- Sys.Date() - as.Date(substr(pac_description(pac,
                                                   version = descr[["Version"]],
                                                   lib.loc = lib.loc)[["Date/Publication"]], 1, 10))
      if (identical(life, structure(numeric(0), class = "difftime", units = "days"))) life <- NA
      return(life)
    }
    } else {
      life <- Sys.Date() - as.Date(substr(pac_description(pac,
                                                   version = last_version,
                                                   lib.loc = lib.loc)[["Date/Publication"]], 1, 10))
      return(life)
    }
  }

  ison_cran <- is_cran(pac)

  if (ison_cran) {
    if (is.null(version) && !is.null(at)) {
      pac_tm <- utils::tail(pac_timemachine(pac, at = at), 1)
      pac_tm$LifeDuration
    } else {
      if (isTRUE(utils::compareVersion(version, last_version) == 1)) {
        return(NA)
      }
      pac_tm <- pac_timemachine(pac)
      if (isTRUE(all(vapply(pac_tm$Version, function(v) isFALSE(utils::compareVersion(v, version) == 0), logical(1))))) return(NA)
      pac_tm <- pac_tm[vapply(pac_tm$Version, function(v) isTRUE(utils::compareVersion(v, version) == 0), logical(1)), ]
      pac_tm$LifeDuration
    }
  } else{
    NA
  }
}

#' CRAN package health state at a specific Date or for a specific version
#' @description using CRAN website to get a package version/versions used at a specific Date interval.
#' A healthy package was published for more than x days, where default is 14 days.
#' CRAN team gives around one/two week to resolved a package which gave errors under the check page.
#' @param pac character a package name.
#' @param version character version of package. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @param limit numeric at least days to treat as healthy. Default: 14
#' @param scope character vector scope of R CRAN check pages statuses to consider, any of `c("ERROR", "FAIL", "WARN", "NOTE")`. Default `c("ERROR", "FAIL")`
#' @param flavors character vector of CRAN machines to consider, which might be retrieved with `pacs::cran_flavors()$Flavor`. By default all CRAN machines are considered, NULL value. Default NULL
#' @param lib.loc character vector. Is omitted for non NULL version. Default: NULL
#' @param repos character the base URL of the repository to use. Default "https://cran.rstudio.org"
#' @return logical if package is healthy.
#' @note Function will scrap two/tree CRAN URLS. Works only with CRAN packages.
#' The newest release are checked for warnings/errors on R CRAN check page.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' Results are cached for 1 hour with `memoise` package, memory cache.
#' @export
#' @examples
#' \dontrun{
#' pac_health("memoise")
#' pac_health("dplyr", version = "0.8.0", limit = 14)
#' pac_health("dplyr", limit = 14, scope = c("ERROR", "FAIL"))
#' }
pac_health <- function(pac,
                       version = NULL,
                       at = NULL,
                       limit = 14,
                       scope = c("ERROR", "FAIL"),
                       flavors = NULL,
                       lib.loc = NULL,
                       repos = "https://cran.rstudio.com/") {
  stopifnot(length(pac) == 1 && is.character(pac))
  stopifnot(!all(c(!is.null(version), !is.null(at))))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))

  if (pac %in% pacs_base()) {
    return(TRUE)
  }

  if (any(c(!is.null(version), !is.null(at))) && !pac %in% rownames(available_packages(repos = repos))) {
    return(NA)
  }

  poss_pacs <- unique(c(rownames(installed_packages(lib.loc = lib.loc)),
                        rownames(available_packages(repos = repos))))

  if (all(c(is.null(version), is.null(at))) && !pac %in% poss_pacs) {
    return(NA)
  }

  life <- pac_lifeduration(pac, version = version, at = at, lib.loc = lib.loc, repos = repos)

  if (length(life) != 1 || is.na(life)) {
    return(NA)
  }

  res <- isTRUE(life >= limit)

  if (is_last_release(pac, version, at)) {
    if (isTRUE(pac_checkred(pac, scope = scope, flavors = flavors))) FALSE else res
  } else {
    res
  }
}
