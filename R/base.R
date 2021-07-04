#' Package dependencies from DESCRIPTIONS files.
#' @description Package dependencies from DESCRIPTIONS files.
#' @param pac character a package name.
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @param lib.loc character vector. Is omitted for non NULL version., Default: NULL
#' @param attr logical specify if pac and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: TRUE
#' @param base logical if to add base packages too. Default: FALSE
#' @param description_v if the dependecies version should be taken from description files, minimal required. Default: FALSE
#' @return data.frame with packages and their versions. Versions are taken from `installed.packages`.
#' @export
#' @examples
#' pacs::pac_deps("stats", base = TRUE)$Package
#' \dontrun{
#' pacs::pac_deps("shiny")$Package
#' pacs::pac_deps("shiny", description_v = FALSE)
#' }
pac_deps <- function(pac,
                     fields = c("Depends", "Imports", "LinkingTo"),
                     lib.loc = NULL,
                     base = FALSE,
                     description_v = FALSE,
                     attr = TRUE) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(is.logical(base))
  stopifnot(is.logical(attr))
  stopifnot(pac %in% c(rownames(utils::installed.packages(lib.loc = lib.loc)), pacs_base()))

  paks_global <- NULL
  pac_v <- utils::packageDescription(pac, lib.loc = lib.loc)$Version

  deps <- function(pak, fileds) {
    pks <- utils::packageDescription(pak)
    res <- NULL
    for (f in fileds) {
      ff <- pks[[f]]
      if (!is.null(ff)) {
        res <- c(
          res,
          vapply(
            strsplit(trimws(strsplit(ff, ",")[[1]]), "[ \n\\(]"),
            function(x) x[1],
            character(1)
          )
        )
      }
    }
    if (is.null(res)) {
      return(NULL)
    }
    for (r in res) {
      if (r != "R" && !r %in% paks_global) {
        paks_global <<- c(r, paks_global)
        deps(r, fields)
      }
    }
  }

  deps(pac, fields)

  res <- unique(c(
    setdiff(
      paks_global,
      c(
        pac,
        if (!base) {
          c(pacs_base(), "R")
        } else {
          NULL
        }
      )
    ),
    if (!attr) {
      pac
    } else {
      NULL
    }
  ))

  if (description_v) {
    res_df <- installed_descriptions(lib.loc, fields)
    res_df <- res_df[res_df$Package %in% res, ]
  } else {
    res_df <- as.data.frame(utils::installed.packages(lib.loc = lib.loc)[res, c("Package", "Version"), drop = FALSE])
  }

  if (attr) {
    attr(res_df, "Package") <- pac
    attr(res_df, "Version") <- pac_v
  }

  res_df
}

#' compare specific package versions
#' @description compare specific package versionss.
#' @param pacs character vector of packages.
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @param lib.loc character vector. Is omitted for non NULL version., Default: NULL
#' @param attr logical specify if pac and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: FALSE
#' @param base logical if to add base packages too. Default: FALSE
#' @param description_v if the dependecies version should be taken from description files, minimal required. Default: FALSE
#' @return data.frame
#' @export
#' @examples
#' pacs_deps(c("stats", "base"), base = TRUE, attr = FALSE)
#'
pacs_deps <- function(pacs = NULL,
                      fields = c("Depends", "Imports", "LinkingTo"),
                      lib.loc = NULL,
                      attr = TRUE,
                      base = FALSE,
                      description_v = FALSE
                      ) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(is.null(pacs) || is.character(pacs))

  if(!is.null(pacs)) {
    tocheck <- pacs
  } else {
    tocheck <- rownames(utils::installed.packages(lib.loc = lib.loc))
  }

  dfs <- do.call(rbind, lapply(seq_along(tocheck), function(x) pac_deps(tocheck[x],
                                                             fields = fields,
                                                             lib.loc = lib.loc,
                                                             attr = attr,
                                                             base = base,
                                                             description_v)))

  if (nrow(dfs) > 0) {
    # higher version have a priority
    stats::aggregate(dfs[, c("Version"), drop = FALSE], list(Package = dfs$Package),  compareVersionsMax)
  } else {
    dfs
  }
}
