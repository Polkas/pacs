#' Package dependencies from DESCRIPTIONS files.
#' @description
#' @param pac character a package name.
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @param lib.loc character vector. Is omitted for non NULL version., Default: NULL
#' @param version character version of the package. Other than NULL triggers remote mode and omit lib.loc. Default: NULL
#' @param attr logical specify if pac and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: NULL
#' @param repos character cran URL. Default: "http://cran.rstudio.com/"
#' @note will temporarily download two packages if version argument is specified.
#' @return data.frame
#' @export
#' @examples
#' pac_deps("stats")
#' \dontrun{
#' pac_deps("shiny", version = "1.6.0)
#' }
#'

pac_deps <- function(pac,
                     version = NULL,
                     fields = c("Depends", "Imports", "LinkingTo"),
                     lib.loc = NULL,
                     attr = TRUE,
                     repos = "http://cran.rstudio.com/") {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(all(fields %in% c("Depends", "Imports", "LinkingTo", "Suggests")))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))

  withr::with_temp_libpaths({
  if (!is.null(version)) {
    cat(sprintf("Please wait %s %s is downloaded, TEMPORARLY.\n", pac, version))
    devtools::install_version(pac,
                              version,
                              force = FALSE,
                              dependencies = FALSE,
                              quiet = TRUE,
                              upgrade = "always",
                              repos = repos)
    lib.loc <- NULL
  } else {
    stopifnot(all(pac %in% rownames(utils::installed.packages(lib.loc = lib.loc))))
  }

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
            strsplit(ss, "[ \n]"),
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
              if(length(rr)) rr else ""
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
  })
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
#'
pac_compare_versions <- function(pac,
                                 old,
                                 new,
                                 repos = "http://cran.rstudio.com/") {

  stopifnot(utils::compareVersion(new, old) == 1)

  s_remote <- pac_deps(pac, version = old, repos = repos)
  s_remote2 <- pac_deps(pac, version = new, repos = repos)

  res <- merge(s_remote, s_remote2, by = c("Package"), all = TRUE, suffix = paste0(".", c(old, new)))

  res_df <- suppressWarnings(res[replaceNA(as.character(res[[paste0("Package_raw.", old)]]), "NA") != replaceNA(as.character(res[[paste0("Package_raw.", new)]]), "NA"), ])
  res_df <- res_df[order(res_df$Package), ]
  rownames(res_df) <- NULL
  res_df
}


#' compare specific package versions
#' @description using remote
#' @param pacs character vector of packages.
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @param lib.loc character vector. Is omitted for non NULL version., Default: NULL
#' @param versions character version of the package. Other than NULL triggers remote mode and omit lib.loc. Default: NULL
#' @param repos character cran URL. Default: "http://cran.rstudio.com/"
#' @note will temporarily download packages if version argument is specified.
#' @return data.frame
#' @export
#' @examples
#' pacs_deps(c("stats", "base"))
#'
#' \dontrun{
#' pacs_deps(c("shiny", "cat2cat"), versions = c("1.6.0", "0.2.1"))
#' }
#'
pacs_deps <- function(pacs = NULL,
                      versions = NULL,
                      fields = c("Depends", "Imports", "LinkingTo"),
                      lib.loc = NULL,
                      attr = TRUE,
                      repos = "http://cran.rstudio.com/") {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(is.null(versions) || length(pacs) == length(versions))

  if(!is.null(pacs)) {
    tocheck <- pacs
  } else {
    tocheck <- rownames(utils::installed.packages(lib.loc = lib.loc))
    versions <- NULL
  }

  dfs <- do.call(rbind, lapply(seq_along(tocheck), function(x) pac_deps(tocheck[x],
                                                             version = versions[x],
                                                             fields = fields,
                                                             lib.loc = lib.loc,
                                                             attr = FALSE,
                                                             repos = repos)))

  # higher version have a priority
  stats::aggregate(dfs[, c("Version"), drop = FALSE], list(Package = dfs$Package),  compareVersionsMax)
}

#' size of the package
#' @description using remote
#' @param pacs character vector of local packages.
#' @param version character version of the package. Other than NULL triggers remote mode and omit lib.loc. Default: NULL
#' @param lib.loc character vector. Default: NULL
#' @param repos character cran URL. Default: "http://cran.rstudio.com/"
#' @return data.frame
#' @export
#' @examples
#' cat(pacs::pacs_size("stats")/10**6, "Mb")
#'

pac_size <- function(pac, version = NULL, lib.loc = NULL, repos = "http://cran.rstudio.com/") {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))

  withr::with_temp_libpaths({

    if (!is.null(version)) {
      cat(sprintf("Please wait %s %s is downloaded, TEMPORARLY.\n", pac, version))
      devtools::install_version(pac,
                                version,
                                force = FALSE,
                                dependencies = FALSE,
                                quiet = TRUE,
                                upgrade = "always",
                                repos = repos)
      lib.loc <- NULL
    } else {
      stopifnot(all(pac %in% rownames(utils::installed.packages(lib.loc = lib.loc))))
    }

   dir_size(find.package(pac, lib.loc = lib.loc))

  })

}

#' size of packages
#' @description using remote
#' @param pacs character vector of local packages.
#' @param versions character version of the package. Other than NULL triggers remote mode and omit lib.loc. Default: NULL
#' @param lib.loc character vector. Default: NULL
#' @return data.frame
#' @export
#' @examples
#' cat(pacs::pacs_size("stats")/10**6, "Mb")
#'

pacs_size <- function(pacs = NULL, versions = NULL, lib.loc = NULL) {
  stopifnot(is.null(pacs) || is.character(pacs))
  stopifnot(all(pacs %in% rownames(utils::installed.packages())))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(is.null(versions) || length(pacs) == length(versions))

  if(!is.null(pacs)) {
    tocheck <- pacs
  } else {
    tocheck <- rownames(utils::installed.packages(lib.loc = lib.loc))
    versions <- NULL
  }

  dirs <- vapply(seq_along(pacs),
                 function(p) pac_size(tocheck[p], version = versions[p], lib.loc = lib.loc),
                 numeric(1))
  sum(dirs)

}

#' Package true size
#' @description xxx
#' @param pac character a local package name.
#' @param fields character vector, Default: c("Depends", "Imports", "LinkingTo")
#' @param lib.loc character vector, Default: NULL
#' @return data.frame
#' @export
#' @examples
#' # size in Mb
#' pacs::pac_true_size("stats")/10**6
#'
pac_true_size <- function(pac, version = NULL, fields = c("Depends", "Imports", "LinkingTo"), lib.loc = NULL) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(pac %in% rownames(utils::installed.packages(lib.loc = lib.loc)))
  stopifnot(all(fields %in% c("Depends", "Imports", "LinkingTo", "Suggests")))

  pacs_size(setdiff(pac_deps(pac, version = version, fields = fields, lib.loc = lib.loc, attr = FALSE)$Package, "R"))
}

#' Compare current and expected packages under .libPaths.
#' @description CHecking the healthy of the libarary.
#' @param lib.loc character. Default: NULL
#' @return data.frame
#' @export
#' @examples
#' \dontrun{
#' validate_lib
#' }
#'

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
