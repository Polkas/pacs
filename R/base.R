#' Package dependencies from DESCRIPTIONS files.
#' @description Package dependencies from DESCRIPTIONS files.
#' @param pac character a package name.
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @param lib.loc character vector. Is omitted for non NULL version., Default: NULL
#' @param attr logical specify if pac and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: TRUE
#' @return data.frame
#' @export
#' @examples
#' pac_deps("stats")
pac_deps <- function(pac,
                     fields = c("Depends", "Imports", "LinkingTo"),
                     lib.loc = NULL,
                     attr = TRUE) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(all(fields %in% c("Depends", "Imports", "LinkingTo", "Suggests")))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(pac %in% rownames(utils::installed.packages(lib.loc = lib.loc)))

  paks_global <- list()
  pac_v <- utils::packageDescription(pac, lib.loc = lib.loc)$Version

  deps <- function(pak, lib.loc = NULL, fileds) {
    pks <- suppressWarnings(utils::packageDescription(pak, lib.loc = lib.loc))
    if (length(pks) == 1 && is.na(pks)) {
      return(NULL)
    }
    res <- NULL
    vv <- NULL
    for (f in fileds) {
      ff <- pks[[f]]
      if (!is.null(ff)) {
        ss <- trimws(strsplit(ff, ",")[[1]])
        res <- c(
          res,
          vapply(
            strsplit(ss, "[ \n\\(]"),
            function(x) x[1],
            character(1)
          )
        )
        vv <- c(
          vv,
          vapply(
            ss,
            function(x) {
              rr <- regmatches(x, regexec("([0-9\\.-]+)\\)", x, perl = TRUE))[[1]][2]
              if (length(rr)) rr else ""
            },
            character(1)
          )
        )
      }
    }
    if (is.null(res)) {
      return(NULL)
    }
    for (iter in  seq_along(res)) {
      paks_global[[res[iter]]] <<- if (vv[iter]  != "" && !is.null(paks_global[[res[iter]]]) && paks_global[[res[iter]]] != "" && !utils::compareVersion(vv[iter], paks_global[[res[iter]]]))  paks_global[[res[iter]]] else vv[iter]
      deps(res[iter], lib.loc, fileds)
    }
  }

  deps(pac, lib.loc, fields)
  res_df <- data.frame(
    Package = names(paks_global),
    Version = unlist(paks_global),
    Package_raw = vapply(paks_global,
                       function(x) names(x),
                       character(1)))
  res_df$Version[is.na(res_df$Version)] <- ""
  if (attr) {
    attr(res_df, "Package") <- pac
    attr(res_df, "Version") <- pac_v
  } else {
    base_package <- data.frame(Package = pac, Version = as.character(pac_v), Package_raw = "")
    res_df <- rbind(res_df, base_package)
  }

  res_df <- res_df[order(res_df$Package), ]
  rownames(res_df) <- NULL
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
#' pac_compare_versions("shiny", "1.4.0-2", "1.6.0")
#' }
pac_compare_versions <- function(pac,
                                 old,
                                 new,
                                 repos = "http://cran.rstudio.com/") {

  stopifnot(utils::compareVersion(new, old) == 1)

  withr::with_temp_libpaths({
    cat(sprintf("Please wait %s %s is downloaded, TEMPORARLY.\n", pac, old))
    devtools::install_version(pac,
      old,
      force = TRUE,
      dependencies = FALSE,
      quiet = TRUE,
      upgrade = "always",
      repos = repos
    )
    s_remote <- pac_deps(pac)
  })

  withr::with_temp_libpaths({
    cat(sprintf("Please wait %s %s is downloaded, TEMPORARLY.\n", pac, new))
    devtools::install_version(pac,
      new,
      force = TRUE,
      dependencies = FALSE,
      quiet = TRUE,
      upgrade = "always",
      repos = repos
    )
    s_remote2 <- pac_deps(pac)
  })

  res <- merge(s_remote, s_remote2, by = c("Package"), all = TRUE, suffix = paste0(".", c(old, new)))
  col_old <- paste0("Package_raw.", old)
  col_new <- paste0("Package_raw.", new)
  res_df <- suppressWarnings(res[replaceNA(as.character(res[[col_old]]), "NA") != replaceNA(as.character(res[[col_new]]), "NA"), ])
  res_df <- res_df[order(res_df$Package), ]
  rownames(res_df) <- NULL
  res_df
}


#' compare specific package versions
#' @description compare specific package versionss.
#' @param pacs character vector of packages.
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @param lib.loc character vector. Is omitted for non NULL version., Default: NULL
#' @param attr logical specify if pac and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: FALSE
#' @return data.frame
#' @export
#' @examples
#' pacs_deps(c("stats", "base"))
#'
#' \dontrun{
#' pacs_deps(c("shiny", "cat2cat"), versions = c("1.6.0", "0.2.1"))
#' }
pacs_deps <- function(pacs = NULL,
                      fields = c("Depends", "Imports", "LinkingTo"),
                      lib.loc = NULL,
                      attr = FALSE) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))

  if(!is.null(pacs)) {
    tocheck <- pacs
  } else {
    tocheck <- rownames(utils::installed.packages(lib.loc = lib.loc))
  }

  dfs <- do.call(rbind, lapply(seq_along(tocheck), function(x) pac_deps(tocheck[x],
                                                             fields = fields,
                                                             lib.loc = lib.loc,
                                                             attr = attr)))

  # higher version have a priority
  stats::aggregate(dfs[, c("Version"), drop = FALSE], list(Package = dfs$Package),  compareVersionsMax)
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
#' @return data.frame
#' @export
#' @examples
#' # size in Mb
#' pacs::pac_true_size("stats")/10**6
pac_true_size <- function(pac, fields = c("Depends", "Imports", "LinkingTo"), lib.loc = NULL) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "LinkingTo", "Suggests")))
  pacs_all <- pac_deps(pac, fields = fields, lib.loc = lib.loc, attr = FALSE)
  sum(pacs_size(setdiff(pacs_all$Package, "R"), lib.loc = lib.loc))
}

#' Compare current and expected packages under .libPaths.
#' @description Checking the healthy of the libarary.
#' @param lib.loc character. Default: NULL
#' @return data.frame
#' @export
#' @examples
#' \dontrun{
#' validate_lib()
#' }
validate_lib <- function(lib.loc = NULL) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))

  pp <- pacs_deps(lib.loc = lib.loc)

  ii_df <- as.data.frame(utils::installed.packages(lib.loc = lib.loc))
  ii_res <-  rbind(stats::aggregate(ii_df[, c("Version"), drop = FALSE], list(Package = ii_df$Package), function(x) x[1]),
                   c("R", paste(R.Version()[c("major", "minor")], collapse = ".")))

  res <- merge(ii_res, pp, by = c("Package"), suffix = c(".have", ".minimal"), all = TRUE)

  res2 <- suppressWarnings(res[replaceNA(as.character(res$Version.have), "NA") != replaceNA(as.character(res$Version.minimal), "NA"), ])
  res2
}
