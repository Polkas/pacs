#' package DESCRIPTION file
#' @description CRAN package DESCRIPTION file taken locally or remotely from GITHUB CRAN mirror or CRAN website.
#' @param pac character a package name.
#' @param version character package version, By default the newest version in taken if failed tried to give local one if installed. Default: NULL
#' @param at Date. Default: NULL
#' @param local logical if to use local library. Default: FALSE
#' @param lib.loc character vector, used optionally when local is equal TRUE. Default: `.libPaths()`
#' @param repos character the base URL of the CRAN repository to use. Used only for the validation. Default `https://cran.rstudio.com/`
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
                            lib.loc = .libPaths(),
                            repos = "https://cran.rstudio.com/") {
  stopifnot(isFALSE(local) ||
    (isTRUE(local) && (is.null(version) || isTRUE(utils::packageDescription(pac, lib.loc = lib.loc)$Version == version))))
  stopifnot(all(c(is.null(version), is.null(at))) || xor(!is.null(version), !is.null(at)))
  stopifnot(is.null(at) || inherits(at, "Date"))
  stopifnot(length(pac) == 1 && is.character(pac))
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))

  is_installed <- isTRUE(pac %in% rownames(installed_packages(lib.loc = lib.loc)))

  if (!is_installed && (!pac_isin(pac, repos) || (!is.null(version) && isTRUE(utils::compareVersion(version, pac_last(pac)) == 1)))) {
    return(structure(list(), package = pac, version = version))
  }

  if ((local) && (is.null(version) || (!is.null(version) && isTRUE(utils::packageDescription(pac, lib.loc = lib.loc)$Version == version)))) {
    if (!is_installed) {
      return(structure(list(), package = pac, version = version))
    }
    result <- utils::packageDescription(pac, lib.loc)
    return(structure(result, package = pac, version = result$version))
  } else {
    result <- pac_description_dcf(pac, version, at)
    if (length(result) == 0 && is_installed && is.null(version)) {
      result <- utils::packageDescription(pac, lib.loc)
      version <- result$Version
      return(structure(result, package = pac, version = version))
    } else {
      return(structure(result, package = pac, version = attr(result, "version")))
    }
  }
}

pac_description_dcf_raw <- function(pac, version, at) {
  if (!is.null(at)) {
    tt <- pac_timemachine(pac, at = at)
    version <- utils::tail(tt[order(tt$LifeDuration), ], 1)$Version
  }

  ee <- tempfile()

  last_version <- pac_last(pac)

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
    if (isTRUE(!is.null(version) && version != last_version)) {
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

    download <- try(
      {
        suppressWarnings(utils::download.file(d_url,
          destfile = temp_tar,
          quiet = TRUE
        ))
      },
      silent = TRUE
    )

    if (inherits(download, "try-error")) {
      return(structure(list(), package = pac, version = version))
    }

    temp_dir <- tempdir(check = TRUE)
    utils::untar(temp_tar, exdir = temp_dir)
    # tabs are not acceptable
    result <- as.list(read.dcf(file.path(temp_dir, pac, "DESCRIPTION"))[1, ])
    unlink(temp_dir, recursive = TRUE)
  } else {
    result <- as.list(read.dcf(ee)[1, ])
    unlink(ee)
  }

  structure(result, package = pac, version = version)
}

pac_description_dcf <- memoise::memoise(pac_description_dcf_raw, cache = cachem::cache_mem(max_age = 60 * 60))
