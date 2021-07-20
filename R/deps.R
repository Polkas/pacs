#' Package dependencies
#' @description Package dependencies from DESCRIPTION files with installed or expected versions or newest released.
#' @param pac character a package name.
#' @param fields character vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lib.loc character vector. Is omitted for non NULL version., Default: NULL
#' @param attr logical specify if a package and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: TRUE
#' @param base logical if to add base packages too. Default: FALSE
#' @param local logical if to use newest CRAN packages, where by default local ones are used. Default: TRUE
#' @param description_v if the dependencies version should be taken from description files, minimal required. Default: FALSE
#' @return data.frame with packages and their versions. Versions are taken from `installed.packages` or newest released.
#' @export
#' @examples
#' pacs::pac_deps("stats", base = TRUE)$Package
#' pacs::pac_deps("memoise")$Package
#' pacs::pac_deps("memoise", description_v = FALSE)
#'
pac_deps <- function(pac,
                     fields = c("Depends", "Imports", "LinkingTo"),
                     lib.loc = NULL,
                     base = FALSE,
                     local = TRUE,
                     description_v = FALSE,
                     attr = TRUE) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(is.logical(base))
  stopifnot(is.logical(attr))
  stopifnot(!all(c(!local, description_v)))

  if (local) {
    stopifnot(pac %in% c(rownames(utils::installed.packages(lib.loc = lib.loc)), pacs_base()))

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
      }
      for (r in res) {
        if (r != "R" && !r %in% paks_global) {
          paks_global <<- c(r, paks_global)
          deps(r, fields)
        }
      }
    }

    deps(pac, fields)
    v_base <- utils::installed.packages(lib.loc = lib.loc)
  } else {
    stopifnot(pac %in% rownames(available_packages()))
    paks_global <- tools::package_dependencies(pac,
      db = available_packages(),
      which = fields,
      recursive = TRUE
    )[[1]]
    v_base <- available_packages()
    pac_v <- v_base[pac, c("Version")]
  }

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
    res_df <- as.data.frame(v_base[res, c("Package", "Version"), drop = FALSE])
  }

  if (attr) {
    attr(res_df, "Package") <- pac
    attr(res_df, "Version") <- pac_v
  }

  res_df
}

#' Packages dependencies
#' @description Package dependencies from DESCRIPTION files with installed or expected versions or newest released.
#' @param pacs character vector of packages.
#' @param fields character vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lib.loc character vector. Is omitted for non NULL version., Default: NULL
#' @param attr logical specify if package and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: FALSE
#' @param base logical if to add base packages too. Default: FALSE
#' @param local logical if to use newest CRAN packages, where by default local ones are used. Default: TRUE
#' @param description_v if the dependencies version should be taken from description files, minimal required. Default: FALSE
#' @return data.frame
#' @export
#' @examples
#' pacs_deps(c("stats", "base"), base = TRUE, attr = FALSE)
pacs_deps <- function(pacs = NULL,
                      fields = c("Depends", "Imports", "LinkingTo"),
                      lib.loc = NULL,
                      attr = TRUE,
                      base = FALSE,
                      local = TRUE,
                      description_v = FALSE) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(is.null(pacs) || is.character(pacs))

  if (!is.null(pacs)) {
    tocheck <- pacs
  } else {
    tocheck <- rownames(utils::installed.packages(lib.loc = lib.loc))
  }

  dfs <- do.call(rbind, lapply(seq_along(tocheck), function(x) {
    pac_deps(tocheck[x],
      fields = fields,
      lib.loc = lib.loc,
      attr = attr,
      base = base,
      local = local,
      description_v
    )
  }))

  if (nrow(dfs) > 0) {
    # higher version have a priority
    stats::aggregate(dfs[, c("Version"), drop = FALSE], list(Package = dfs$Package), compareVersionsMax)
  } else {
    dfs
  }
}
