#' package dependencies
#' @description Package dependencies from DESCRIPTION files with installed or expected versions or newest released.
#' @param pac character a package name.
#' @param fields character vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lib.loc character vector. Is omitted for non NULL version. Default: NULL
#' @param base logical if to add base packages too. Default: FALSE
#' @param local logical if to use newest CRAN packages, where by default local ones are used. Default: TRUE
#' @param description_v if the dependencies version should be taken from description files, minimal required. Default: FALSE
#' @param attr logical specify if a package and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: TRUE
#' @param recursive logical if to assess the dependencies recursively. Default: TRUE
#' @param repos character the base URL of the CRAN repository to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return data.frame with packages and their versions. Versions are taken from `installed.packages` or newest released.
#' @note When function is invoked in the loop afterwards binded results could be aggregated like,
#' `stats::aggregate(results[, c("Version"), drop = FALSE], list(Package = results$Package), pacs::compareVersionsMax)`.
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_deps("stats", base = TRUE)$Package
#' pacs::pac_deps("memoise")$Package
#' pacs::pac_deps("memoise", description_v = FALSE)
#' # raw dependencies from DESCRIPTION file
#' pacs::pac_deps("memoise", description_v = TRUE, recursive = FALSE)
#' # raw dependencies from DESCRIPTION file - last release
#' pacs::pac_deps("memoise", description_v = TRUE, local = FALSE, recursive = FALSE)
#' }
pac_deps <- function(pac,
                     fields = c("Depends", "Imports", "LinkingTo"),
                     lib.loc = NULL,
                     base = FALSE,
                     local = TRUE,
                     description_v = FALSE,
                     attr = TRUE,
                     recursive = TRUE,
                     repos = "https://cran.rstudio.com/") {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(is.logical(base))
  stopifnot(is.logical(attr))
  stopifnot(is.logical(recursive))
  stopifnot(is.character(repos))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))

  if (local) {
    stopifnot(pac %in% c(rownames(installed_packages(lib.loc = lib.loc)), pacs_base()))

    paks_global <- NULL
    pac_v <- pac_description(pac, local = TRUE, lib.loc = lib.loc)$Version

    deps <- function(pak, fileds) {
      pks <- pac_description(pak, local = TRUE, lib.loc = lib.loc)
      ff <- paste(unlist(pks[fields]), collapse = ", ")
      fff <- strsplit(trimws(strsplit(ff, ",")[[1]]), "[ \n\\(]")
      res <- NULL
      if (length(fff) > 0) {
        res <- vapply(
          fff,
          function(x) x[1],
          character(1)
        )
      }
      if (is.null(res)) {
        return(NULL)
      } else {
        res <- setdiff(res, "NA")
      }
      for (r in res) {
        if (isFALSE(r == "R") && isFALSE(r %in% paks_global)) {
          paks_global <<- c(r, paks_global)
          if (recursive) deps(r, fields)
        }
      }
    }

    deps(pac, fields)
    v_base <- installed_agg_fun(lib.loc, fields)
  } else {
    stopifnot(pac_isin(pac, repos))
    paks_global <- tools::package_dependencies(pac,
      db = available_packages(repos),
      which = fields,
      recursive = recursive
    )[[1]]
    v_base <- available_packages(repos)
    pac_v <- v_base[pac, c("Version")]
  }

  res <- unique(c(
    if (base) {
      c(paks_global, pacs_base())
    } else {
      setdiff(paks_global, c(pacs_base()))
    },
    if (!attr) {
      pac
    } else {
      NULL
    }
  ))

  if (description_v) {
    if (local) {
      res_df <- installed_descriptions(lib.loc, fields, if (recursive) unique(c(res, pac)) else pac)
    } else {
      res_df <- available_descriptions(repos, fields, if (recursive) unique(c(res, pac)) else pac)
    }
    res_df <- res_df[res_df$Package %in% res, ]
  } else {
    res_df <- as.data.frame(v_base[v_base[, "Package"] %in% res, c("Package", "Version"), drop = FALSE])
  }

  if (attr) {
    attr(res_df, "Package") <- pac
    attr(res_df, "Version") <- pac_v
  }

  rownames(res_df) <- NULL

  res_df
}
