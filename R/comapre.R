#' Compare dependencies of specific packages versions
#' @description using remote CRAN to compare specific packages versions dependencies.
#' @param pac character a package name.
#' @param old character an old version of package.
#' @param new character a new version of package.
#' @param fields character a vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @return data.frame with 4 columns.
#' @export
#' @examples
#' pac_compare_versions("memoise", "0.2.1", "2.0.0")
pac_compare_versions <- function(pac,
                                 old,
                                 new,
                                 fields = c("Imports", "Depends", "LinkingTo")) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(utils::compareVersion(new, old) == 1)

  one_base <- paste(Filter(function(x) length(x) > 0, pac_description(pac, version = old)[fields]), collapse = ",")
  one_e <- extract_deps(one_base)
  s_remote <- unique(data.frame(
    Package = one_e$packages[[1]], Version = replaceNA(one_e$versions[[1]], ""),
    stringsAsFactors = FALSE
  ))

  two_base <- paste(Filter(function(x) length(x) > 0, pac_description(pac, version = new)[fields]), collapse = ",")
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
