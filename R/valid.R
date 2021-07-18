
#' Compare current and expected packages under .libPaths.
#' @description Checking the healthy of the libarary.
#' @param lib.loc character. Default: NULL
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @param lifeduration logical if to add life duration column. Default: FALSE
#' @return data.frame with 5/6 columns Package Version.expected.min Version.have. "" means newest version.
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' @export
#' @examples
#' lib_validate()
lib_validate <- function(lib.loc = NULL,
                         fields = c("Depends", "Imports", "LinkingTo"),
                         lifeduration = FALSE) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(is.logical(lifeduration))
  installed_agg <- installed_agg_fun(lib.loc, fields)

  res_agg <- installed_descriptions(lib.loc, fields)

  result <- merge(res_agg,
    rbind(
      installed_agg[, c("Package", "Version")],
      data.frame(
        Package = "R",
        Version = paste0(R.Version()[c("major", "minor")], collapse = "."), stringsAsFactors = FALSE
      )
    ),
    by = "Package",
    all = TRUE,
    suffix = c(".expected.min", ".have")
  )

  result$version_status <- apply(result, 1, function(x) utils::compareVersion(x["Version.have"], x["Version.expected.min"]))

  result <- result[!is.na(result$Package) & !(result$Package %in% c("NA", pacs_base())), ]

  if (lifeduration) result$life_duration <- apply(result, 1, function(x) pac_lifeduration(x["Package"], x["Version.have"]))

  result$newest <- apply(result, 1, function(x) is_last_release(x["Package"], x["Version.have"]))

  result
}

#' Compare current and expected package dependencies versions using DESCRIPTION files.
#' @description Checking the healthy of the specific packages.
#' @param pac character a package name.
#' @param lib.loc character. Default: NULL
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @return data.frame with 3 columns Package Version.expected.min Version.have. "" means newest version.
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' @export
#' @examples
#' pac_validate("memoise")
pac_validate <- function(pac, lib.loc = NULL, fields = c("Depends", "Imports", "LinkingTo")) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot((length(pac) == 1) && is.character(pac))

  descriptions_pac <- pac_deps(pac, lib.loc = lib.loc, fields = fields, description_v = TRUE)
  installed_pac <- pac_deps(pac, lib.loc = lib.loc, fields = fields)

  result <- merge(descriptions_pac,
    installed_pac,
    by = "Package",
    all = TRUE,
    suffix = c(".expected.min", ".have")
  )

  result$version_status <- apply(result, 1, function(x) utils::compareVersion(x["Version.have"], x["Version.expected.min"]))

  result <- result[!is.na(result$Package) & !(result$Package %in% c("NA", pacs_base())), ]

  result$life_duration <- vapply(result$Package, pac_lifeduration, numeric(1))

  result$newest <- apply(result, 1, function(x) is_last_release(x["Package"], x["Version.have"]))

  result
}

#' Compare current and expected packages dependencies versions using DESCRIPTION files.
#' @description Checking the healthy of the specific packages.
#' @param pacs character vector packages names.
#' @param lib.loc character. Default: NULL
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @return data.frame with 3 columns Package Version.expected.min Version.have. "" means newest version.
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' @export
#' @examples
#' pacs_validate(c("memoise", "rlang"))
pacs_validate <- function(pacs, lib.loc = NULL, fields = c("Depends", "Imports", "LinkingTo")) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(is.null(pacs) || is.character(pacs))

  descriptions_pac <- pacs_deps(pacs, lib.loc = lib.loc, fields = fields, description_v = TRUE)
  installed_pac <- pacs_deps(pacs, lib.loc = lib.loc, fields = fields)

  result <- merge(descriptions_pac,
    installed_pac,
    by = "Package",
    all = TRUE,
    suffix = c(".expected.min", ".have")
  )

  result
}
