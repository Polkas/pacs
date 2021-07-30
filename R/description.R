#' package DESCRIPTION file
#' @description package DESCRIPTION file taken locally or if remotely then from GITHUB CRAN mirror or CRAN website.
#' @param pac character a package name.
#' @param version character package version. Default: NULL
#' @param at Date. Default: NULL
#' @param local logical if to use local library. Default: FALSE
#' @param lib.loc character used optionally when local is equal TRUE. Default: NULL
#' @return list with names proper for DESCRIPTION file fields.
#' @note Results are cached for 1 hour with `memoise` package.
#' @export
#' @examples
#' pac_description("dplyr", version = "0.8.0")
#' pac_description("dplyr", at = as.Date("2019-02-01"))
pac_description <- function(pac, version = NULL, at = NULL, local = FALSE, lib.loc = NULL) {
  stopifnot(isFALSE(local) ||
    (isTRUE(local) && (is.null(version) || isTRUE(utils::packageDescription(pac)$Version == version))))
  stopifnot(all(c(is.null(version), is.null(at))) || xor(!is.null(version), !is.null(at)))
  stopifnot(is.null(at) || inherits(at, "Date"))
  stopifnot(length(pac) == 1)

  if (local && (is.null(version) || (!is.null(version) && isTRUE(utils::packageDescription(pac)$Version == version)))) {
    return(utils::packageDescription(pac, lib.loc))
  } else {
    pac_description_dcf(pac, version, at)
  }
}

pac_description_dcf_raw <- function(pac, version, at) {
  if (!is.null(at)) {
    tt <- pac_timemachine(pac, at = at)
    version <- utils::tail(tt[order(tt$Life_Duration), ], 1)$Version
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
    last_version <- last_version_fun(pac)
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

    utils::download.file(d_url,
      destfile = temp_tar,
      quiet = TRUE
    )

    temp_dir <- tempdir(check = TRUE)

    utils::untar(temp_tar, exdir = temp_dir)
    # tabs are not acceptable

    as.list(read.dcf(file.path(temp_dir, pac, "DESCRIPTION"))[1, ])
  } else {
    as.list(read.dcf(ee)[1, ])
  }
}

pac_description_dcf <- memoise::memoise(pac_description_dcf_raw, cache = cachem::cache_mem(max_age = 60 * 60))
