#' package DESCRIPTION file
#' @description package DESCRIPTION file taken from CRAN website.
#' @param pac character a package name.
#' @param version character package version. Default: NULL
#' @param at Date. Default: NULL
#' @param local logical if to use local library. Default: FALSE
#' @param lib.loc character used optionally when local is equal TRUE. Default: NULL
#' @return numeric size in bytes.
#' @export
#' @examples
#' # Will fail
#' # pac_description("dplyr", version = "0.8.0")
#' # pac_description("dplyr", at = as.Date("2019-02-01"))
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

pac_description_dcf <- function(pac, version, at) {

  if (!is.null(at)) {
    tt <- pac_timemachine(pac, at = at)
    version <- utils::tail(tt[order(tt$Life_Duration), ], 1)$Version
  }

  ee = tempfile()

  last_version <- available_packages[rownames(available_packages) == pac, "Version"]

  if (is.null(version)) {
    version <- last_version
  }

  d_url <- sprintf("https://raw.githubusercontent.com/cran/%s/%s/DESCRIPTION",
                   pac,
                   version)

  utils::download.file(d_url,
                       destfile = ee,
                       quiet = TRUE)

  as.list(read.dcf(ee)[1, ])
}

#' packages DESCRIPTION files
#' @description packages DESCRIPTION files taken from CRAN website.
#' @param pacs character vector packages names.
#' @param versions character vector versions. Default: NULL
#' @param at Date . Default: NULL
#' @param local logical if to use local library. Default: FALSE
#' @param lib.loc character used optionally when local is equal TRUE. Default: NULL
#' @return numeric size in bytes.
#' @export
#' @examples
#' # pacs_description(c("dplyr", "shiny"), version = c("0.8.1", "1.5.0"))
#' # pacs_description(c("dplyr", "shiny"), at = as.Date("2019-02-01"))
pacs_description <- function(pacs, versions = NULL, at = NULL, local = FALSE, lib.loc = NULL) {
  stopifnot(is.null(versions) || length(pacs) == length(versions))
  stopifnot(all(c(is.null(versions), is.null(at))) || xor(!is.null(versions), !is.null(at)))
  stopifnot(is.null(at) || inherits(at, "Date"))

  stats::setNames(
    lapply(
      seq_along(pacs),
      function(x) pac_description(pacs[x], version = versions[x], at = at, local = local, lib.loc = lib.loc)
    ),
    pacs
  )
}
