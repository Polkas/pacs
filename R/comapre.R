#' compare dependencies of specific package versions
#' @description using remote cran to compare specific package versions dependencies.
#' @param pac character a package name.
#' @param old character old version of package.
#' @param new character new version of package.
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
                                 repos = "http://cran.rstudio.com/") {

  stopifnot(utils::compareVersion(new, old) == 1)

  withr::with_temp_libpaths({
    cat(sprintf("Please wait %s %s is downloaded, TEMPORARLY.\n", pac, old))
    remotes::install_version(pac,
                             old,
                             force = TRUE,
                             dependencies = FALSE,
                             quiet = TRUE,
                             upgrade = "always",
                             repos = repos
    )
    s_remote <- pac_deps(pac, description_v = TRUE)
  })

  withr::with_temp_libpaths({
    cat(sprintf("Please wait %s %s is downloaded, TEMPORARLY.\n", pac, new))
    remotes::install_version(pac,
                             new,
                             force = TRUE,
                             dependencies = FALSE,
                             quiet = TRUE,
                             upgrade = "always",
                             repos = repos
    )
    s_remote2 <- pac_deps(pac, description_v = TRUE)
  })

  res <- merge(s_remote, s_remote2, by = c("Package"), all = TRUE, suffix = paste0(".", c(old, new)))
  col_old <- paste0("Version.", old)
  col_new <- paste0("Version.", new)
  res_df <- suppressWarnings(res[replaceNA(as.character(res[[col_old]]), "NA") != replaceNA(as.character(res[[col_new]]), "NA"), ])
  rownames(res_df) <- NULL
  res_df
}
