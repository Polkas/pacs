#' Package version life duration at specific Date or for a specific version
#' @description using CRAN website to get a package life duration for certain version or at a specific Date.
#' @param pac character a package name.
#' @param version character package version, By default the newest version is taken.
#' The local repository has priority, it version is available. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @param lib.loc character vector. Is omitted for non NULL version. Default: `.libPaths()`
#' @param repos character vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor. Default `pacs::biocran_repos()`
#' @param source character one of `c("crandb", "cran")`. Using the `MEATCRAN  CRANDB` or the direct web page download from CRAN. Default: `"crandb"`
#' @return `difftime`, number of days package version was the newest one.
#' @note Function will scrap two github CRAN mirror and CRAN URL. Works mainly with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' Results are cached for 30 minutes with `memoise` package, memory cache.
#' The `crandb` R packages database is a part of `METACRAN` project, source:
#' Csárdi G, Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages. `https://github.com/r-hub/pkgsearch`, `https://r-hub.github.io/pkgsearch/`.
#' @export
#' @examples
#' \dontrun{
#' pac_lifeduration("memoise")
#' pac_lifeduration("dplyr", version = "0.8.0")
#' pac_lifeduration("dplyr", at = as.Date("2019-02-14"))
#' # For Bioconductor packages will work only for the newest per R version and installed packages.
#' pac_lifeduration("S4Vectors")
#' }
pac_lifeduration <- function(pac,
                             version = NULL,
                             at = NULL,
                             lib.loc = .libPaths(),
                             repos = biocran_repos(),
                             source = c("crandb", "cran")) {
  stopifnot(length(pac) == 1 && is.character(pac))
  stopifnot(!all(c(!is.null(version), !is.null(at))))
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))
  if (!is_online()) {
    return(NA)
  }

  source <- match.arg(source)

  ison_cran <- is_isin(pac, "https://cran.rstudio.com/")
  last_version <- pac_last(pac, repos = repos)
  if (isNA(last_version)) {
    return(NA)
  }

  is_installed <- isTRUE(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
  version_installed <- if (is_installed) {
    pac_description(pac, local = TRUE)$Version
  } else {
    NA
  }
  version <- if (!is.null(version)) {
    version
  } else if (!is.null(at)) {
    utils::tail(pac_timemachine(pac, at = at)$Version, 1)
  } else {
    last_version
  }

  is_the_last_version <- isTRUE(utils::compareVersion(version, last_version) == 0)
  is_the_installed_version <- isTRUE(utils::compareVersion(version, version_installed) == 0)

  if (is_installed && is_the_installed_version && is_the_last_version) {
    descr <- utils::packageDescription(pac, lib.loc = lib.loc)
    date_start <- descr[["Date/Publication"]]
    if (is.null(date_start)) {
      return(structure(0, class = "difftime"))
    }
    return(Sys.Date() - as.Date(as.character(substr(date_start, 1, 10))))
  } else if (ison_cran && is_the_last_version) {
    return(Sys.Date() - as.Date(substr(
      pac_description(pac,
        version = last_version,
        lib.loc = lib.loc
      )[["Date/Publication"]],
      1, 10
    )))
  } else if (ison_cran) {
    life <- pac_timemachine(pac, version = version, source = source)
    if (isNA(life) || isTRUE(nrow(life) == 0)) {
      return(NA)
    }
    return(structure(life$LifeDuration, class = "difftime"))
  } else {
    return(NA)
  }
}

#' CRAN package health state at a specific Date or for a specific version
#' @description using CRAN website to get a package version/versions used at a specific Date interval.
#' A healthy package was published for more than x days, where default is 14 days.
#' CRAN team gives around one/two week to resolved a package which gave errors under the check page.
#' @param pac character a package name.
#' @param version character package version, By default the newest version is taken. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @param limit numeric at least days to treat as healthy, ">=limit". Default: 14
#' @param scope character vector scope of R CRAN check pages statuses to consider, any of `c("ERROR", "FAIL", "WARN", "NOTE")`. Default `c("ERROR", "FAIL")`
#' @param flavors character vector of CRAN machines to consider, which might be retrieved with `pacs::cran_flavors()$Flavor`. By default all CRAN machines are considered, NULL value. Default NULL
#' @param lib.loc character vector. Is omitted for non NULL version. Default: `.libPaths()`
#' @param repos character the base CRAN URL of the repository to use. Default "https://cran.rstudio.org"
#' @param source character one of `c("crandb", "cran")`. Using the `MEATCRAN` DB or the direct web page download from CRAN. Default: `"crandb"`
#' @return logical if a package is healthy.
#' @note Function will scrap two/tree CRAN URLS. Works only with CRAN packages.
#' The newest release are checked for warnings/errors on R CRAN check page.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' Results are cached for 30 minutes with `memoise` package, memory cache.
#' The `crandb` R packages database is a part of `METACRAN` project, source:
#' Csárdi G, Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages. `https://github.com/r-hub/pkgsearch`, `https://r-hub.github.io/pkgsearch/`.
#' @export
#' @examples
#' \dontrun{
#' pac_health("memoise")
#' pac_health("dplyr", version = "0.8.0", limit = 14)
#' pac_health("dplyr", at = as.Date("2019-02-14"))
#' pac_health("dplyr", limit = 14, scope = c("ERROR", "FAIL"))
#' }
pac_health <- function(pac,
                       version = NULL,
                       at = NULL,
                       limit = 14,
                       scope = c("ERROR", "FAIL"),
                       flavors = NULL,
                       lib.loc = .libPaths(),
                       repos = "https://cran.rstudio.com/",
                       source = c("crandb", "cran")) {
  stopifnot(length(pac) == 1 && is.character(pac))
  stopifnot(!all(c(!is.null(version), !is.null(at))))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))
  stopifnot(length(repos) == 1 && is.character(repos))
  if (!is_online()) {
    return(NA)
  }

  source <- match.arg(source)

  if (pac %in% pacs_base()) {
    return(TRUE)
  }

  life <- pac_lifeduration(pac, version = version, at = at, lib.loc = lib.loc, source = source, repos = repos)
  if (isNA(life)) {
    return(NA)
  }
  res <- life >= limit

  if ((length(scope) > 0) && identical(pac_last(pac, repos = repos), version)) {
    if (isTRUE(pac_checkred(pac, scope = scope, flavors = flavors))) FALSE else res
  } else {
    res
  }
}

#' Packages life duration for a specific version
#' @description using CRAN website to get a package life duration for certain version or at a specific Date.
#' @param pacs character vector packages names.
#' @param versions character vector packages versions.
#' @param source character one of `c("crandb", "loop_crandb", "loop_cran")`. Default: `"crandb"`
#' @param lib.loc character vector. Is omitted for non NULL version. Default: `.libPaths()`
#' @param repos character the base CRAN URL of the repository to use. Default `"https://cran.rstudio.org"`
#' @param pacs character vector packages names.
#' @return data.frame with two columns package name and life duration.
#' @note Function will scrap two/tree CRAN URLS. Works only with CRAN packages.
#' Results are cached for 30 minutes with `memoise` package, memory cache.
#' The `crandb` R packages database is a part of `METACRAN` project, source:
#' Csárdi G, Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages. `https://github.com/r-hub/pkgsearch`, `https://r-hub.github.io/pkgsearch/`.
#' @export
#' @examples
#' \dontrun{
#' pacs_lifeduration(c("dplyr", "tidyr"), c("1.0.0", "1.2.0"))
#' pacs_lifeduration(c("dplyr", "tidyr"), c("1.0.0", "1.2.0"), source = "loop_cran")
#' }
pacs_lifeduration <- function(pacs, versions, source = c("crandb", "loop_crandb", "loop_cran"), lib.loc = .libPaths(), repos = biocran_repos()) {
  if (length(pacs) != length(versions)) {
    return(rep(NA, length(pacs)))
  }
  source <- match.arg(source)

  if (source == "crandb" && length(pacs) <= getOption("pacs.crandb_limit", 100)) {
    meta <- crandb_json(pacs)
    ll <- lapply(names(meta), function(x) {
      tl <- meta[[x]]$timeline
      res <- data.frame(Package = x, Version = names(tl), Released = as.Date(substr(tl, 1, 10)), stringsAsFactors = FALSE)
      res <- res[order(res$Released), ]
      res$LifeDuration <- diff(c(res$Released, Sys.Date()))
      res
    })
    names(ll) <- names(meta)

    ld <- sapply(names(ll), function(x) {
      if (!isNA(vv <- versions[pacs == x])) ll[[x]][ll[[x]]$Version == vv, "LifeDuration"][1] else NA
    })

    result <- data.frame(Package = names(meta), lifeduration = ld, stringsAsFactors = FALSE)
  } else {
    ld <- vapply(
      seq_along(pacs),
      function(x) {
        if (!isNA(version_p <- versions[x])) {
          pac_lifeduration(pacs[x],
            version_p,
            repos = repos,
            lib.loc = lib.loc,
            source = `if`(source == "loop_cran", "cran", "crandb")
          )
        } else {
          NA
        }
      }, numeric(1)
    )
    result <- data.frame(Package = pacs, lifeduration = ld, stringsAsFactors = FALSE)
  }
  rownames(result) <- NULL
  result
}
