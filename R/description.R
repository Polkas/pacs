#' Package DESCRIPTION file
#' @description package DESCRIPTION file taken locally or remotely from GITHUB CRAN mirror or CRAN website.
#' @param pac character a package name.
#' @param version character package version. Default: NULL
#' @param at Date. Default: NULL
#' @param local logical if to use local library. Default: FALSE
#' @param lib.loc character used optionally when local is equal TRUE. Default: NULL
#' @param repos character the base URL of the repository to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return list with names proper for DESCRIPTION file fields.
#' @note Results are cached for 1 hour with `memoise` package.
#' @export
#' @examples
#' \dontrun{
#' pac_description("dplyr", version = "0.8.0")
#' pac_description("dplyr", at = as.Date("2019-02-01"))
#' }
pac_description <- function(pac,
                            version = NULL,
                            at = NULL,
                            local = FALSE,
                            lib.loc = NULL,
                            repos = "https://cran.rstudio.com/") {
  stopifnot(isFALSE(local) ||
    (isTRUE(local) && (is.null(version) || isTRUE(utils::packageDescription(pac)$Version == version))))
  stopifnot(all(c(is.null(version), is.null(at))) || xor(!is.null(version), !is.null(at)))
  stopifnot(is.null(at) || inherits(at, "Date"))
  stopifnot(length(pac) == 1 && is.character(pac))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))

  if (!pac %in% rownames(available_packages(repos = repos)) || (!is.null(version) && isTRUE(utils::compareVersion(version, pac_last(pac)) == 1))) {
    return(list())
  }

  if (local && (is.null(version) || (!is.null(version) && isTRUE(utils::packageDescription(pac)$Version == version)))) {
    if (!pac %in% installed_packages(lib.loc = lib.loc)) return(list())
    return(utils::packageDescription(pac, lib.loc))
  } else {
    pac_description_dcf(pac, version, at)
  }
}

pac_description_dcf_raw <- function(pac, version, at) {
  if (!is.null(at)) {
    tt <- pac_timemachine(pac, at = at)
    version <- utils::tail(tt[order(tt$LifeDuration), ], 1)$Version
  }

  ee <- tempfile()

  last_version <- last_version_fun(pac)

  if (is.null(version)) {
    version <- last_version
  }

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
    temp_tar <- tempfile(fileext = "tar.gz")
    if (!is.null(version) && version != last_version) {
      base_url <- sprintf("https://cran.r-project.org/src/contrib/Archive/%s", pac)
    } else {
      base_url <- "https://cran.r-project.org/src/contrib"
      version <- last_version
    }
    d_url <- sprintf(
      "%s/%s_%s.tar.gz",
      base_url,
      pac,
      version
    )

    download <- try({
      utils::download.file(d_url,
        destfile = temp_tar,
        quiet = TRUE
      )}, silent = TRUE)

    if (inherits(download, "try-error")) return(list())

    temp_dir <- tempdir(check = TRUE)
    utils::untar(temp_tar, exdir = temp_dir)
    # tabs are not acceptable
    result <- as.list(read.dcf(file.path(temp_dir, pac, "DESCRIPTION"))[1, ])
    unlink("temp_dir", recursive = TRUE)
  } else {
    result <- as.list(read.dcf(ee)[1, ])
    unlink(ee)
  }

  result
}

pac_description_dcf <- memoise::memoise(pac_description_dcf_raw, cache = cachem::cache_mem(max_age = 60 * 60))
