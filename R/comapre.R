#' compare dependencies of specific package versions
#' @description using remote cran to compare specific package versions dependencies.
#' @param pac character a package name.
#' @param old character old version of package.
#' @param new character new version of package.
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @param repos character cran URL. Default: "http://cran.rstudio.com/"
#' @return data.frame
#' @note Function will temporarily download two packages.
#' @export
#' @examples
#' \dontrun{
#' pac_compare_versions("shiny", "1.4.0", "1.6.0")
#' }
pac_compare_versions <- function(pac,
                                 old,
                                 new,
                                 fields = c("Imports", "Depends", "LinkingTo"),
                                 repos = "http://cran.rstudio.com/") {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(utils::compareVersion(new, old) == 1)

  one_base <- paste(pac_description(pac, version = old)[fields], collapse = ",")
  one_e <- extract_deps(one_base)
  s_remote <- data.frame(Package = one_e$packages[[1]], Version = replaceNA(one_e$versions[[1]], ""), stringsAsFactors = FALSE)

  two_base <- paste(pac_description(pac, version = new)[fields], collapse = ",")
  two_e <- extract_deps(two_base)
  s_remote2 <- data.frame(Package = two_e$packages[[1]], Version = replaceNA(two_e$versions[[1]], ""), stringsAsFactors = FALSE)

  res <- merge(s_remote, s_remote2, by = c("Package"), all = TRUE, suffix = paste0(".", c(old, new)))
  col_old <- paste0("Version.", old)
  col_new <- paste0("Version.", new)
  res$Same <- replaceNA(as.character(res[[col_old]]), "NA") == replaceNA(as.character(res[[col_new]]), "NA")
  rownames(res) <- NULL
  res[order(res$Same), ]
}
