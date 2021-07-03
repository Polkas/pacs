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

#' size of the package
#' @description size of package.
#' @param pac character a package name.
#' @param lib.loc character vector. Default: NULL
#' @return data.frame
#' @export
#' @examples
#' cat(pacs::pacs_size("stats")/10**6, "Mb")
pac_size <- function(pac, lib.loc = NULL) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(pac %in% rownames(utils::installed.packages(lib.loc = lib.loc)))
  found <- try(find.package(pac, lib.loc = lib.loc), silent = TRUE)
  if (inherits(found, "try-error")){
    0
  } else {
    dir_size(found)
  }
}

#' size of packages.
#' @description size of packages.
#' @param pacs character vector packages.
#' @param lib.loc character vector. Default: NULL
#' @return data.frame
#' @export
#' @examples
#' cat(pacs::pacs_size("stats")/10**6, "Mb")
pacs_size <- function(pacs = NULL, lib.loc = NULL) {
  stopifnot(is.null(pacs) || is.character(pacs))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))

  if(!is.null(pacs)) {
    tocheck <- pacs
  } else {
    tocheck <- rownames(utils::installed.packages(lib.loc = lib.loc))
  }

  dirs <- vapply(tocheck,
                 function(p) pac_size(p, lib.loc = lib.loc),
                 numeric(1))

  stats::setNames(dirs, tocheck)
}

#' Package true size
#' @description Package true size as it takes into account dependencies.
#' @param pac character a package name.
#' @param fields character vector, Default: c("Depends", "Imports", "LinkingTo")
#' @param lib.loc character vector, Default: NULL
#' @param base logical if to add base packages too. Default: FALSE
#' @param exclude_joint integer exclude packages which are dependencies of at least N other packages. Default: 0
#' @return data.frame
#' @export
#' @examples
#' # size in Mb
#' pacs::pac_true_size("stats")/10**6
#' # exclude packages if at least one other package use it too
#' \dontrun{
#' pacs::pac_true_size("devtools", exclude_joint = 1L)/10**6
#' }
pac_true_size <- function(pac,
                          fields = c("Depends", "Imports", "LinkingTo"),
                          lib.loc = NULL,
                          base = FALSE,
                          exclude_joint = 0L) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "LinkingTo", "Suggests")))
  stopifnot(is.integer(exclude_joint))

  pacs_all <- pac_deps(pac, fields = fields, lib.loc = lib.loc, attr = FALSE, base = base)$Package

  if (exclude_joint) {
    depsy <- stats::setNames(lapply(pacs_all, function(x) tools::dependsOnPkgs(x)), pacs_all)
    pacs_all <- setdiff(pacs_all, names(Filter(function(x) length(x) > exclude_joint, depsy)))
  }

  sum(pacs_size(setdiff(pacs_all, "R"), lib.loc = lib.loc))

}

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

