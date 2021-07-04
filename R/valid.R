#' Compare current and expected packages under .libPaths.
#' @description Checking the healthy of the libarary.
#' @param lib.loc character. Default: NULL
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @return data.frame with 3 columns Package Version.expected.min Version.have. "" means newest version.
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' @export
#' @examples
#' lib_validate()
lib_validate <- function(lib.loc = NULL, fields = c("Depends", "Imports", "LinkingTo")) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))

  installed_agg <- installed_agg_fun(lib.loc, fields)

  res_agg <- installed_descriptions(lib.loc, fields)

  result <- merge(res_agg,
                  installed_agg[, c("Package", "Version")],
                  by = "Package",
                  all = TRUE,
                  suffix = c(".expected.min", ".have"))
  result
}

installed_descriptions <- function(lib.loc, fields) {

  installed_agg <- installed_agg_fun(lib.loc, fields)

  paks <- installed_agg[, fields]
  nams <- rownames(paks)
  rownames(paks) <- nams

  df_split <- lapply(strsplit(apply(paks, 1, function(x) paste(x, sep=",")), ","), trimws)
  versions <- lapply(df_split, function(x) sapply(regmatches(x, regexec("([0-9\\.-]+)\\)", x, perl= TRUE)), function(x) `[`(x, 2)))
  packages <- lapply(df_split, function(x) sapply(strsplit(x, "[ \n\\(]"), function (x) `[`(x, 1)))

  joint <- do.call(rbind, lapply(seq_len(length(packages)),
                                 function(x) data.frame(Version = replaceNA(versions[[x]], ""),
                                                        Package = replace(packages[[x]], versions[[x]] == "NA", NA),
                                                        stringsAsFactors = FALSE)))
  res_agg <- stats::aggregate(joint[, c("Version"), drop = FALSE],
                              list(Package = joint$Package),
                              compareVersionsMax)

  res_agg$Version[is.na(res_agg$Version)] <- ""

  res_agg
}


installed_agg_fun <- function(lib.loc = NULL, fields) {
  installed_df <- as.data.frame(utils::installed.packages(lib.loc = NULL))
  installed_agg <- stats::aggregate(installed_df[ , c("Version", fields), drop = FALSE],
                                    list(Package = installed_df$Package),
                                    function(x) x[1])
  installed_agg
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
#' \dontrun{
#' pac_validate("devtools")
#' }
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
                  suffix = c(".expected.min", ".have"))

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
#' \dontrun{
#' pacs_validate(c("devtools", "renv"))
#' }
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
                  suffix = c(".expected.min", ".have"))

  result

}
