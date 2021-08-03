#' Compare DESCRIPTION files dependencies between specific packages versions
#' @description using the remote github CRAN mirror to compare DESCRIPTION files dependencies between specific packages versions.
#' @param pac character a package name.
#' @param old character an old version of package, default local version. Default: NULL
#' @param new character a new version of package, default newest version. Default: NULL
#' @param fields character a vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lib.loc character. Default: NULL
#' @param repos character the base URL of the repositories to use. Default `https://cran.rstudio.com/`
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
#' pac_compare_versions("memoise", "0.2.1", "2.0.0")
#' pac_compare_versions("memoise", "0.2.1")
pac_compare_versions <- function(pac,
                                 old = NULL,
                                 new = NULL,
                                 fields = c("Imports", "Depends", "LinkingTo"),
                                 lib.loc = NULL,
                                 repos = "https://cran.rstudio.com/") {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.null(old) || (length(old) == 1) && is.character(old))
  stopifnot(is.null(new) || (length(new) == 1) && is.character(new))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))

  if (is.null(old)) {
    stopifnot(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
    old <- pac_description(pac, local = TRUE)$Version
  }

  if (is.null(new)) {
    new <- pac_last(pac)
  }

  if (utils::compareVersion(new, old) < 0) {
    return(data.frame())
  }

  one_base <- paste(Filter(function(x) length(x) > 0, pac_description(pac, version = old, lib.loc = lib.loc, repos = repos)[fields]), collapse = ",")
  one_e <- extract_deps(one_base)
  s_remote <- unique(data.frame(
    Package = one_e$packages[[1]], Version = replaceNA(one_e$versions[[1]], ""),
    stringsAsFactors = FALSE
  ))

  two_base <- paste(Filter(function(x) length(x) > 0, pac_description(pac, version = new, lib.loc = lib.loc, repos = repos)[fields]), collapse = ",")
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
  res
}

#' Compare NAMESPACE exports between specific packages versions
#' @description using the remote github CRAN mirror to compare NAMESPACE exports between specific packages versions.
#' @param pac character a package name.
#' @param old character an old version of package.
#' @param new character a new version of package.
#' @param lib.loc character. Default: NULL
#' @param repos character the base URL of the repositories to use. Default `https://cran.rstudio.com/`
#' @return list with two slots, added and removed
#' @note The comparison is only about exports.
#' @export
#' @examples
#' pac_compare_exports("memoise", "0.2.1", "2.0.0")
#' pac_compare_exports("shiny", "1.4.0", "1.6.0")
pac_compare_exports <- function(pac,
                                old = NULL,
                                new = NULL,
                                lib.loc = NULL,
                                repos = "https://cran.rstudio.com/") {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.null(old) || (length(old) == 1) && is.character(old))
  stopifnot(is.null(new) || (length(new) == 1) && is.character(new))

  if (is.null(old)) {
    stopifnot(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
    old <- pac_description(pac, local = TRUE)$Version
  }

  if (is.null(new)) {
    new <- pac_last(pac)
  }

  if (utils::compareVersion(new, old) < 0) {
    return(list())
  }

  old_e <- pac_namespace(pac, old, lib.loc = lib.loc, repos = repos)$exports
  new_e <- pac_namespace(pac, new, lib.loc = lib.loc, repos = repos)$exports

  cat(sprintf("%s:%s vs %s\n", pac, old, new))
  list(removed = setdiff(old_e, new_e), added = setdiff(new_e, old_e))

}
