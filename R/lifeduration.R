#' Package life duration at specific Date or for a specific version
#' @description using CRAN website to get a package life duration for certain version or at a specific Date.
#' @param pac character a package name.
#' @param version character version of package. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @return logical if package is healthy.
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' Results are cached for 1 hour with `memoise` package, memory cache.
#' @export
#' @examples
#' pac_lifeduration("memoise")
#' pac_lifeduration("dplyr", version = "0.8.0")
pac_lifeduration <- function(pac, version = NULL, at = NULL) {
  stopifnot(length(pac) == 1)
  stopifnot(!all(c(!is.null(version), !is.null(at))))

  if (!pac %in% rownames(available_packages())) {
    return(NA)
  } else {
    last_version <- last_version_fun(pac)
  }

  is_installed <- isTRUE(pac %in% rownames(utils::installed.packages()))

  if ((is.null(version) && is.null(at)) ||
    (!is.null(version) && isTRUE(utils::compareVersion(version, last_version) == 0))) {

    if (is_installed) {
    descr <- utils::packageDescription(pac)
    if (isTRUE(utils::compareVersion(last_version, descr[["Version"]]) == 0)) {
      life <- Sys.Date() - as.Date(descr[["Date/Publication"]])
      return(life)
    } else {
      life <- Sys.Date() - as.Date(pac_description(pac, version = descr[["Version"]])[["Date/Publication"]])
      return(life)
    }
    } else {
      life <- Sys.Date() - as.Date(pac_description(pac, version = last_version)[["Date/Publication"]])
      return(life)
    }
  }

  if (is.null(version)) {
    pac_tm <- utils::tail(pac_timemachine(pac, at = at), 1)
    pac_tm$Life_Duration
  } else {
    pac_tm <- pac_timemachine(pac)
    if (any(vapply(pac_tm$Version, function(v) isTRUE(utils::compareVersion(v, version) == 0), logical(1)))) return(NA)
    pac_tm <- pac_tm[vapply(pac_tm$Version, function(v) isTRUE(utils::compareVersion(v, version) == 0), logical(1)), ]
    pac_tm$Life_Duration
  }
}

#' Packages life duration at specific Date or for a specific version
#' @description using CRAN website to get packages life duration for versions or specific Date.
#' @param pacs character vector packages names.
#' @param versions character vector versions of packages. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @return logical if package is healthy.
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' Results are cached for 1 hour with `memoise` package, memory cache.
#' @export
#' @examples
#' pacs_lifeduration(c("memoise", "rlang"))
#' pacs_lifeduration("dplyr", version = "0.8.0")
pacs_lifeduration <- function(pacs, versions = NULL, at = NULL) {
  stopifnot(!all(c(!is.null(versions), !is.null(at))))
  stopifnot(is.null(versions) || length(pacs) == length(versions))

  stats::setNames(
    lapply(
      seq_along(pacs),
      function(x) pac_lifeduration(pacs[x], version = versions[x], at = at)
    ),
    pacs
  )
}

#' Package health state at a specific Date or for a specific version
#' @description using CRAN website to get a package version/versions used at a specific Date interval.
#' A healthy package is a if it was published for more than x days where default is 14 days.
#' CRAN team gives around one/two week to resolved a package which gave errors under the check page.
#' @param pac character a package name.
#' @param version character version of package. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @param limit numeric at least days to treat as healthy. Default: 14
#' @return logical if package is healthy.
#' @note Function will scrap two/tree CRAN URLS. Works only with CRAN packages.
#' The newest release are checked for warnings/errors on R CRAN check page.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' Results are cached for 1 hour with `memoise` package, memory cache.
#' @export
#' @examples
#' pac_health("memoise")
#' pac_health("dplyr", version = "0.8.0")
pac_health <- function(pac, version = NULL, at = NULL, limit = 14) {
  stopifnot(length(pac) == 1)
  stopifnot(!all(c(!is.null(version), !is.null(at))))

  if (pac %in% pacs_base()) {
    return(TRUE)
  }

  if (any(c(!is.null(version), !is.null(at))) && !pac %in% rownames(available_packages())) {
    return(NA)
  }

  poss_pacs <- unique(c(rownames(utils::installed.packages()),
                        rownames(available_packages())))

  if (all(c(is.null(version), is.null(at))) && !pac %in% poss_pacs) {
    return(NA)
  }

  life <- pac_lifeduration(pac, version = version, at = at)

  if (is.na(life)) {
    return(NA)
  }

  res <- life >= limit

  if (is_last_release(pac, version, at)) {
    if (pac_checkred(pac)) FALSE else res
  } else {
    res
  }
}

#' Packages health state at a specific Date or for a specific versions
#' @description using CRAN website to get a package version/versions used at a specific Date interval.
#' A healthy package is a if it was published for more than 14 days.
#' CRAN team gives around one week to resolved a package which gave errors under the check page.
#' @param pacs character vector packages names.
#' @param versions character vector versions of packages. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @return logical vector, TRUE if a package is healthy.
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' Results are cached for 1 hour with `memoise` package, memory cache.
#' @export
#' @examples
#' pacs_health(c("memoise"))
#' pacs_health(c("memoise", "devtools"), versions = c("1.0.0", "2.4.0"))
pacs_health <- function(pacs, versions = NULL, at = NULL) {
  stopifnot(!all(c(!is.null(versions), !is.null(at))))
  stopifnot(is.null(versions) || length(pacs) == length(versions))

  stats::setNames(
    lapply(
      seq_along(pacs),
      function(x) pac_health(pacs[x], version = versions[x], at = at)
    ),
    pacs
  )
}
