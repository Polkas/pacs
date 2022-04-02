#' package DESCRIPTION file
#' @description CRAN package DESCRIPTION file taken locally or remotely from GITHUB CRAN mirror or CRAN website.
#' @param pac character a package name.
#' @param version character package version, by default the is_installed version. Default: NULL
#' @param at Date. Default: NULL
#' @param local logical if to use local library. Default: FALSE
#' @param lib.loc character vector, used optionally when local is equal TRUE. Default: `.libPaths()`
#' @param repos character the base URL of the CRAN repository to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return list with names proper for DESCRIPTION file fields.
#' @note Results are cached for 30 minutes with `memoise` package.
#' @export
#' @examples
#' \dontrun{
#'   pacs::pac_description("dplyr", version = "0.8.0")
#'   pacs::pac_description("dplyr", at = as.Date("2019-02-01"))
#' }
pac_description <- function(pac,
                            version = NULL,
                            at = NULL,
                            local = FALSE,
                            lib.loc = .libPaths(),
                            repos = "https://cran.rstudio.com/") {
  stopifnot((isFALSE(local)) ||
    (isTRUE(local) && (is.null(version) || isTRUE(utils::packageDescription(pac, lib.loc = lib.loc)$Version == version))))
  stopifnot(all(c(is.null(version), is.null(at))) || xor(!is.null(version), !is.null(at)))
  stopifnot(is.null(at) || inherits(at, "Date"))
  stopifnot(length(pac) == 1 && is.character(pac))
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))

  is_installed <- isTRUE(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
  if ((!is_installed && local) || (!local && !is_online())) {
    return(NA)
  }

  version_installed <- if (is_installed) {
    utils::packageDescription(pac)$Version
  } else {
    NA
  }
  version_null <- is.null(version)

  if (local && is_installed && is.null(at) && (version_null || isTRUE(utils::compareVersion(version, version_installed) == 0))) {
    result <- utils::packageDescription(pac, lib.loc)
  } else if (isTRUE(is_isin(pac, "https://cran.rstudio.com/"))) {
    last_version <- pac_last(pac, repos = repos)
    version <- if (!version_null) {
      version
    } else if (!is.null(at)) {
      vv <- utils::tail(pac_timemachine(pac, at = at)$Version, 1)
      if (isNA(vv) || is.null(vv)) {
        return(NA)
      }
      vv
    } else {
      last_version
    }
    result <- pac_description_dcf(pac, version, repos)
    if (isTRUE(is.na(result))) {
      return(NA)
    }
    if (length(result) == 0) {
      return(NA)
    }
  } else {
    return(NA)
  }
  result
}

pac_description_dcf_raw <- function(pac, version, repos = "https://cran.rstudio.com/") {
  ee <- tempfile()

  d_url <- sprintf(
    "https://raw.githubusercontent.com/cran/%s/%s/DESCRIPTION",
    pac,
    version
  )
  tt <- try(
    {
      suppressWarnings(utils::download.file(d_url,
        destfile = ee,
        quiet = TRUE
      ))
    },
    silent = TRUE
  )
  if (inherits(tt, "try-error")) {
    result <- cran_archive_file(pac, version, repos, "DESCRIPTION")
  } else {
    result <- as.list(read.dcf(ee)[1, ])
  }
  unlink(ee)

  structure(result, package = pac, version = version)
}

pac_description_dcf <- memoise::memoise(pac_description_dcf_raw, cache = cachem::cache_mem(max_age = 30 * 60))
