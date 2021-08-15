#' Compare DESCRIPTION files dependencies between specific CRAN packages versions
#' @description using the remote github CRAN mirror to compare DESCRIPTION files dependencies between specific packages versions.
#' @param pac character a package name.
#' @param old character an old version of package, default local version. Default: NULL
#' @param new character a new version of package, default newest version. Default: NULL
#' @param fields character a vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lib.loc character. Default: NULL
#' @param repos character the base URL of the CRAN repository to use. Default "https://cran.rstudio.org"
#' @return data.frame with 4 columns.
#' \describe{
#' \item{Package}{character package names.}
#' \item{Version.OLD}{character versions of dependencies required by an old package version.}
#' \item{Version.NEW}{character versions of dependencies required by a new package version.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' }
#' @export
#' @examples
#' \dontrun{
#' pac_compare_versions("memoise", "0.2.1", "2.0.0")
#' pac_compare_versions("memoise", "0.2.1")
#' }
pac_compare_versions <- function(pac,
                                 old = NULL,
                                 new = NULL,
                                 fields = c("Imports", "Depends", "LinkingTo"),
                                 lib.loc = NULL,
                                 repos = "https://cran.rstudio.com/") {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(pac_isin(pac, repos))
  stopifnot(is.null(old) || (length(old) == 1) && is.character(old))
  stopifnot(is.null(new) || (length(new) == 1) && is.character(new))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(is.character(repos))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))

  if (is.null(old)) {
    stopifnot(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
    old <- pac_description(pac, local = TRUE)$Version
  }

  if (is.null(new)) {
    new <- pac_last(pac)
  }

  stopifnot(utils::compareVersion(new, old) >= 0)

  one_desc <- pac_description(pac, version = old, lib.loc = lib.loc, repos = repos)
  if (length(one_desc) == 0) stop(sprintf("Version %s is not exists for %s.", old, pac))
  one_base <- paste(Filter(function(x) length(x) > 0, one_desc[fields]), collapse = ",")
  one_e <- extract_deps(one_base)
  s_remote <- unique(data.frame(
    Package = one_e$packages[[1]], Version = replaceNA(one_e$versions[[1]], ""),
    stringsAsFactors = FALSE
  ))

  two_desc <- pac_description(pac, version = new, lib.loc = lib.loc, repos = repos)
  if (length(two_desc) == 0) stop(sprintf("Version %s is not exists for %s.", new, pac))
  two_base <- paste(Filter(function(x) length(x) > 0, two_desc[fields]), collapse = ",")
  two_e <- extract_deps(two_base)
  s_remote2 <- unique(data.frame(
    Package = two_e$packages[[1]], Version = replaceNA(two_e$versions[[1]], ""),
    stringsAsFactors = FALSE
  ))

  res <- merge(s_remote, s_remote2, by = c("Package"), all = TRUE, suffix = paste0(".", c(old, new)))
  col_old <- paste0("Version.", old)
  col_new <- paste0("Version.", new)
  res$version_status <- apply(res, 1, function(x) utils::compareVersion(x[col_new], x[col_old]))
  rownames(res) <- NULL
  attr(res, "package") <- pac
  attr(res, "old") <- old
  attr(res, "new") <- new
  res
}

#' Compare NAMESPACE exports between specific CRAN packages versions
#' @description using the remote github CRAN mirror to compare NAMESPACE exports between specific packages versions.
#' @param pac character a package name.
#' @param old character an old version of package.
#' @param new character a new version of package.
#' @param lib.loc character. Default: NULL
#' @param repos character the base URL of the CRAN repository to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return list with `c("imports", "exports", "exportPatterns", "importClasses", "importMethods", "exportClasses", "exportMethods", "exportClassPatterns", "dynlibs", "S3methods")` slots, and added and removed ones for each of them.
#' @note The comparison is only about exports.
#' @export
#' @examples
#' \dontrun{
#' pac_compare_namespace("shiny", "1.0.0", "1.6.0")
#' pac_compare_namespace("shiny", "1.0.0", "1.6.0")$exports
#' }
pac_compare_namespace <- function(pac,
                                  old = NULL,
                                  new = NULL,
                                  lib.loc = NULL,
                                  repos = "https://cran.rstudio.com/") {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(pac_isin(pac, repos))
  stopifnot(is.null(old) || (length(old) == 1) && is.character(old))
  stopifnot(is.null(new) || (length(new) == 1) && is.character(new))
  stopifnot(is.character(repos))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))

  if (is.null(old)) {
    stopifnot(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
    old <- pac_description(pac, local = TRUE)$Version
  }

  if (is.null(new)) {
    new <- pac_last(pac)
  }

  stopifnot(utils::compareVersion(new, old) >= 0)

  result <- list()
  fields <- c("imports", "exports", "exportPatterns", "importClasses", "importMethods", "exportClasses", "exportMethods", "exportClassPatterns", "dynlibs", "S3methods")

  one_nam <- pac_namespace(pac, old, lib.loc = lib.loc, repos = repos)
  if (length(one_nam) == 0) stop(sprintf("Version %s is not exists for %s.", old, pac))
  two_nam <- pac_namespace(pac, new, lib.loc = lib.loc, repos = repos)
  if (length(two_nam) == 0) stop(sprintf("Version %s is not exists for %s.", new, pac))

  for (f in fields) {
    old_f <- unlist(one_nam[[f]])
    new_f <- unlist(two_nam[[f]])

    result[[f]] <- list(removed = setdiff(old_f, new_f), added = setdiff(new_f, old_f))
  }

  structure(result, package = pac, old = old, new = new)
}
