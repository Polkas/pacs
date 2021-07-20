#' Package life duration at specific Date or for a specific version
#' @description using CRAN website to get a package version/versions used at a specific Date interval.
#' @param pac character a package name.
#' @param version character version of package. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @return logical if package is healthy.
#' If the newest release is published less than 7 days ago then the class of object is "not-sure".
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' pac_lifeduration("dplyr")
#' pac_lifeduration("dplyr", version = "0.8.0")
#'
pac_lifeduration <- function(pac, version = NULL, at = NULL) {
  stopifnot(length(pac) == 1)
  stopifnot(!all(c(!is.null(version), !is.null(at))))

  if (!pac %in% rownames(available_packages())) {
    return(NA)
  }

  last_version <- last_version_fun(pac)

  if ((is.null(version) && is.null(at)) ||
      (!is.null(version) && isTRUE(utils::compareVersion(version, last_version) == 0))) {
    descr <- utils::packageDescription(pac)
    if (isTRUE(utils::compareVersion(last_version, descr[["Version"]]) == 0)) {
      life <- Sys.Date() - as.Date(descr[["Date/Publication"]])
      return(life)
    } else {
      life <- Sys.Date() - as.Date(pac_description(pac, version = descr[["Version"]])[["Date/Publication"]])
      return(life)
    }
  }

  if (is.null(version)) {
    pac_tm <- utils::tail(pac_timemachine(pac, at = at), 1)
    pac_tm$Life_Duration
  } else {
    pac_tm <- pac_timemachine(pac)
    stopifnot(version %in% pac_tm$Version)
    pac_tm <- pac_tm[pac_tm$Version == version, ]
    pac_tm$Life_Duration
  }
}

#' Package health state at a specific Date or for a specific version
#' @description using CRAN website to get a package version/versions used at a specific Date interval.
#' A healthy package is a if it was published for more than 7 days.
#' CRAN team gives around one week to resolved a package which gave errors under the check page.
#' @param pac character a package name.
#' @param version character version of package. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @param limit numeric at least days to treat as healthy. Default: 7
#' @return logical if package is healthy.
#' If the newest release is published less than 7 days ago then the class of object is "not-sure".
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' pac_health("dplyr")
#' pac_health("dplyr", version = "0.8.0")
#'
pac_health <- function(pac, version = NULL, at = NULL, limit = 7) {
  stopifnot(length(pac) == 1)
  stopifnot(!all(c(!is.null(version), !is.null(at))))

  if (!pac %in% rownames(available_packages())) {
    return(NA)
  }

  life <- pac_lifeduration(pac, version = version, at = at)

  res <- life >= limit

  last_version <- last_version_fun(pac)

  if (is_last_release(pac, version, at) && !res) {
    cat(sprintf("This is a newest release of %s published less than 7 days ago so not sure about score.", pac))
    structure(res, class = "not-sure")
  } else {
    structure(res, class = "sure")
  }
}

#' Packages health state at a specific Date or for a specific versions
#' @description using CRAN website to get a package version/versions used at a specific Date interval.
#' A healthy package is a if it was published for more than 7 days.
#' CRAN team gives around one week to resolved a package which gave errors under the check page.
#' @param pacs character vector packages names.
#' @param versions character vector versions of packages. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @return logical vector, TRUE if a package is healthy.
#' If the newest release is published less than 7 days ago then the class of object is "not-sure" for newest version.
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' pacs_health(c("dplyr", "devtools"))
#' pacs_health(c("dplyr", "devtools"), versions = c("0.8.0", "2.4.0"))
#'
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
