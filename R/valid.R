#' Validate the local library
#' @description
#' Checking if installed packages have correct versions taking into account all DESCRIPTION files requirements.
#' Moreover identifying which packages are newest releases.
#' Optionally we could add life duration and CRAN check page status for each package.
#' @param lib.loc character vector. Default: `.libPaths()`
#' @param fields a character vector listing the types of dependencies, a subset of c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances").
#' Character string "all" is shorthand for that vector, character string "most" for the same vector without "Enhances", character string "strong" (default) for the first three elements of that vector.
#' Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lifeduration logical if to assess life duration for each package in the library. `MEATCRAN CRANDB` is used for libraries with less than 500 packages. The direct web page download from CRAN or local evaluation for newest packages otherwise. Default: FALSE
#' @param checkred list with two named fields, `scope` and `flavor`. `scope` of R CRAN check pages statuses to consider, any of `c("ERROR", "FAIL", "WARN", "NOTE")`. `flavor` is a vector of CRAN machines to consider, which might be retrieved with `pacs::cran_flavors()$Flavor`. By default an empty scope field deactivated assessment for `checkred` column, and NULL flavor will results in checking all machines. Default `list(scope = character(0), flavor = NULL)`
#' @param repos character vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' @return data.frame with 4/6/7/8 columns.
#' \describe{
#' \item{Package}{character a package name.}
#' \item{Version.expected.min}{character expected by DESCRIPTION files minimal version. "" means not specified so the newest version.}
#' \item{Version.have}{character installed package version.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' \item{newest}{logical (Internet needed) if the installed version is the newest one. For Bioconductor if is the newest one per R version.}
#' \item{cran}{logical (Internet needed) if the package is on CRAN, version is not taken into account here.}
#' \item{checkred}{(Optional) (Internet needed) logical if the NEWEST package contains any specified statuses on CRAN check page. `pacs::checked_packages` is used to quickly retrieve all statuses at once.}
#' \item{lifeduration}{(Optional) (Internet needed) integer number of days a package was released.}
#' }
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' When turn on the `lifeduration` options, calculations might be time consuming.
#' Results are cached for 30 minutes with `memoise` package.
#' `BioConductor` packages are tested only in available scope, `checkred` is not assessed for them.
#' The `crandb` R packages database is a part of `METACRAN` project, source:
#' Csárdi G, Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages. `https://github.com/r-hub/pkgsearch`, `https://r-hub.github.io/pkgsearch/`.
#' @export
#' @examples
#' \dontrun{
#' lib_validate()
#' lib_validate(checkred = list(scope = c("ERROR", "FAIL", "WARN")))
#' lib_validate(checkred = list(
#'   scope = c("ERROR", "FAIL"),
#'   flavors = cran_flavors()$Flavor[1:2]
#' ))
#' # activate lifeduration argument, could be time consuming for bigger libraries.
#' lib_validate(
#'   lifeduration = TRUE,
#'   checkred = list(scope = c("ERROR", "FAIL"))
#' )
#' # only R CRAN repository
#' lib_validate(repos = "https://cran.rstudio.com/")
#' }
lib_validate <- function(lib.loc = .libPaths(),
                         fields = c("Depends", "Imports", "LinkingTo"),
                         lifeduration = FALSE,
                         checkred = list(scope = character(0), flavors = NULL),
                         repos = biocran_repos()) {
  fields <- expand_dependency(fields)
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(is.list(checkred) &&
    (length(checkred) %in% c(1, 2)) &&
    (c("scope") %in% names(checkred)) &&
    (length(checkred$scope) == 0 || all(checkred$scope %in% c("ERROR", "FAIL", "WARN", "NOTE"))) &&
    (is.null(checkred$flavors) || all(checkred$flavors %in% read_cran_flavours_raw()$Flavor)))
  stopifnot(is.character(repos))
  stopifnot(is.logical(lifeduration))

  installed_agg <- installed_agg_fun(lib.loc, fields)
  res_agg <- installed_descriptions(lib.loc, fields)

  result <- merge(
    res_agg,
    rbind(
      installed_agg[, c("Package", "Version")],
      data.frame(
        Package = "R",
        Version = paste0(R.Version()[c("major", "minor")], collapse = "."), stringsAsFactors = FALSE
      )
    ),
    by = "Package",
    all = TRUE,
    suffix = c(".expected.min", ".have")
  )

  result$version_status <- apply(result, 1, function(x) utils::compareVersion(x["Version.have"], x["Version.expected.min"]))

  result <- result[!is.na(result$Package) & !(result$Package %in% c("", "NA", pacs_base())), ]

  if (is_online()) {
    newest_df <- merge(
      installed_agg[, c("Package", "Version")],
      available_packages(repos = repos)[, c("Package", "Version")],
      by = "Package",
      all.x = TRUE,
      sort = FALSE
    )

    newest_df$newest <- as.character(newest_df$Version.x) == as.character(newest_df$Version.y)

    result <- merge(
      result,
      newest_df[, c("Package", "newest")],
      by = "Package",
      sort = FALSE,
      all.x = TRUE
    )

    cran_df <- merge(installed_agg[, c("Package", "Version")],
      available_packages(repos = "https://cloud.r-project.org")[, c("Package", "Version")],
      by = "Package",
      all.x = TRUE,
      sort = FALSE
    )

    cran_df$cran <- !is.na(cran_df$Version.y)
    cran_df$cran[is.na(cran_df$cran)] <- FALSE

    result <- merge(
      result,
      cran_df[, c("Package", "cran")],
      by = "Package",
      sort = FALSE,
      all.x = TRUE
    )

    if (length(checkred$scope)) {
      checkred_all <- checked_packages()
      if (is.data.frame(checkred_all)) {
        flavors <- if (is.null(checkred$flavors)) grep("r-", colnames(checkred_all)) else checkred$flavors
        scope_final <- c(checkred$scope, paste0(checkred$scope, "*"))
        checkred_all$red_status <- apply(checkred_all[, flavors, drop = FALSE], 1, function(x) any(x %in% scope_final))
        checkred_names_scope <- checkred_all$Package[checkred_all$red_status]
        result$checkred <- (result$Package %in% checkred_names_scope) & result$newest
        result$checkred[is.na(result$newest) | !result$cran] <- NA
      } else {
        message("Failed to retrieve packages statuses with the checked_packages() function.")
        result$checkred <- NA
      }
    }

    if (lifeduration && (nrow(installed_packages(lib.loc = lib.loc)) >= 500)) {
      message("Please wait, Packages life durations are assessed.\n")
      result$lifeduration <- vapply(
        seq_len(nrow(result)),
        function(x) {
          if (!isNA(version_p <- as.character(result[x, "Version.have", drop = TRUE]))) {
            pac_lifeduration(result[x, "Package", drop = TRUE],
              version_p,
              repos = repos,
              lib.loc = lib.loc,
              source = "cran"
            )
          } else {
            NA
          }
        }, numeric(1)
      )
    } else if (lifeduration) {
      ld <- get_crandb_lifedurations(result$Package, result$Version.have)
      result <- merge(result, ld, by = "Package", all.x = TRUE)
    }

    not_installed <- is.na(result$Version.have)
    if (any(not_installed)) {
      result[not_installed, intersect(c("newest", "checkred"), colnames(result))] <- NA
    }
  } else {
    warning("There is no Internet connection.")
  }

  result
}

#' Validate a specific local package
#' @description
#' Checking if installed package dependencies have correct versions taking into account their DESCRIPTION files requirements.
#' Moreover identifying which packages are newest releases.
#' Optionally we could add life duration and CRAN check page status for each dependency.
#' @param pac character a package name.
#' @param lib.loc character vector. Default: `.libPaths()`
#' @param fields a character vector listing the types of dependencies, a subset of c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances").
#' Character string "all" is shorthand for that vector, character string "most" for the same vector without "Enhances", character string "strong" (default) for the first three elements of that vector.
#' Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lifeduration logical if to assess life duration for each package in the library. `MEATCRAN CRANDB` is used for less than 500 packages. The direct web page download from CRAN or local evaluation for newest packages otherwise. Default: FALSE
#' @param checkred list with two named fields, `scope` and `flavor`. `scope` of R CRAN check pages statuses to consider, any of `c("ERROR", "FAIL", "WARN", "NOTE")`. `flavor` vector of machines to consider, which might be retrieved with `pacs::cran_flavors()$Flavor`. By default an empty scope field deactivated assessment for `checkred` column, and NULL flavor will results in checking all machines. Default `list(scope = character(0), flavor = NULL)`
#' @param repos character vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' @return data.frame with 5/7/8/9 columns.
#' \describe{
#' \item{Package}{character a package name.}
#' \item{Version.expected.min}{character expected by DESCRIPTION files minimal version. "" means not specified so the newest version.}
#' \item{Version.have}{character installed package version.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' \item{direct}{ logical if the package is in the first depencency layer, direct depencencies from DESCRIPTION file.}
#' \item{newest}{ logical (Internet needed) if the installed version is the newest one.}
#' \item{cran}{logical (Internet needed) if the package is on CRAN, version is not taken into account here.}
#' \item{checkred}{(Optional) (Internet needed) logical if the NEWEST package contains any specified statuses on CRAN check page.}
#' \item{lifeduration}{(Optional) (Internet needed) integer number of days a package was released.}
#' }
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' When turn on the `lifeduration` option, calculations might be time consuming.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function with `lifeduration` or `checkred` turned on.
#' Results are cached with `memoise` package, memory cache.
#' The `crandb` R packages database is a part of `METACRAN` project, source:
#' Csárdi G, Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages. `https://github.com/r-hub/pkgsearch`, `https://r-hub.github.io/pkgsearch/`.
#' @export
#' @examples
#' \dontrun{
#' pac_validate("memoise")
#' pac_validate("memoise",
#'   lifeduration = TRUE,
#'   checkred = list(scope = c("ERROR", "FAIL"), flavors = NULL)
#' )
#' }
pac_validate <- function(pac,
                         lib.loc = .libPaths(),
                         fields = c("Depends", "Imports", "LinkingTo"),
                         lifeduration = FALSE,
                         checkred = list(scope = character(0), flavors = NULL),
                         repos = biocran_repos()) {
  fields <- expand_dependency(fields)
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.list(checkred) &&
    (length(checkred) %in% c(1, 2)) &&
    (c("scope") %in% names(checkred)) &&
    (length(checkred$scope) == 0 || all(checkred$scope %in% c("ERROR", "FAIL", "WARN", "NOTE"))) &&
    (is.null(checkred$flavors) || all(checkred$flavors %in% read_cran_flavours_raw()$Flavor)))
  stopifnot(is.character(repos))
  stopifnot(is.logical(lifeduration))

  descriptions_pac <- pac_deps(pac, lib.loc = lib.loc, fields = fields, description_v = TRUE)
  descriptions_pac_direct <- pac_deps(pac, lib.loc = lib.loc, fields = fields, description_v = TRUE, recursive = FALSE)

  installed_pac <- pac_deps(pac, lib.loc = lib.loc, fields = fields)

  result <- merge(
    descriptions_pac,
    installed_pac,
    by = "Package",
    all = TRUE,
    suffix = c(".expected.min", ".have")
  )

  if (nrow(result)) {
    result$version_status <- apply(result, 1, function(x) utils::compareVersion(x["Version.have"], x["Version.expected.min"]))

    result <- result[!is.na(result$Package) & !(result$Package %in% c("NA", pacs_base())), ]

    result$direct <- result$Package %in% descriptions_pac_direct$Package
    if (is_online()) {
      result$newest <- apply(result, 1, function(x) isTRUE(pac_islast(x["Package"], version = x["Version.have"], repos = repos)))
      result$cran <- apply(result, 1, function(x) isTRUE(pac_isin(x["Package"], "https://cran.rstudio.com/")))

      if (length(checkred$scope)) {
        checkred_all <- checked_packages()
        if (is.data.frame(checkred_all)) {
          flavors <- if (is.null(checkred$flavors)) grep("r-", colnames(checkred_all)) else checkred$flavors
          scope_final <- c(checkred$scope, paste0(checkred$scope, "*"))
          checkred_all$red_status <- apply(checkred_all[, flavors, drop = FALSE], 1, function(x) any(x %in% scope_final))
          checkred_names_scope <- checkred_all$Package[checkred_all$red_status]
          result$checkred <- (result$Package %in% checkred_names_scope) & result$newest
          result$checkred[is.na(result$newest) | !result$cran] <- NA
        } else {
          message("Failed to retrieve packages statuses with the checked_packages() function.")
          result$checkred <- NA
        }
      }

      if (lifeduration && (nrow(result) >= 500)) {
        message("Please wait, Packages life durations are assessed.\n")
        result$lifeduration <- vapply(
          seq_len(nrow(result)),
          function(x) {
            if (!isNA(version_p <- as.character(result[x, "Version.have", drop = TRUE]))) {
              pac_lifeduration(result[x, "Package", drop = TRUE],
                version_p,
                repos = repos,
                lib.loc = lib.loc,
                source = "cran"
              )
            } else {
              NA
            }
          }, numeric(1)
        )
      } else if (lifeduration) {
        ld <- get_crandb_lifedurations(result$Package, result$Version.have)
        result <- merge(result, ld, by = "Package", all.x = TRUE)
      }

      not_installed <- is.na(result$Version.have)
      if (any(not_installed)) {
        result[not_installed, intersect(c("newest", "checkred"), colnames(result))] <- NA
      }
    } else {
      warning("There is no Internet connection.")
    }
  }

  result
}

#' Validate a specific renv lock file
#' @description
#' This function will be especially useful when renv lock file is built manually.
#' Checking if installed package dependencies have correct versions taking into account their DESCRIPTION files requirements.
#' Moreover identifying which packages are newest releases.
#' Optionally we could add life duration and CRAN check page status for each dependency.
#' @param path character a path to the `renv` lock file.
#' @param lifeduration logical if to assess life duration for each package in the library. `MEATCRAN CRANDB` is used for less than 500 packages. The direct web page download from CRAN or local evaluation for newest packages otherwise. Default: FALSE
#' @param checkred list with two named fields, `scope` and `flavor`. `scope` of R CRAN check pages statuses to consider, any of `c("ERROR", "FAIL", "WARN", "NOTE")`. `flavor` vector of machines to consider, which might be retrieved with `pacs::cran_flavors()$Flavor`. By default an empty scope field deactivated assessment for `checkred` column, and NULL flavor will results in checking all machines. Default `list(scope = character(0), flavor = NULL)`
#' @param repos character vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' @return data.frame with 5/7/8/9 columns.
#' \describe{
#' \item{Package}{character a package name.}
#' \item{Version.expected.min}{character expected by DESCRIPTION files minimal version. "" means not specified so the newest version.}
#' \item{Version.expected}{character package version in the renv lock file.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' \item{direct}{ logical if the package is in the first depencency layer, direct depencencies from DESCRIPTION file.}
#' \item{newest}{ logical (Internet needed) if the installed version is the newest one.}
#' \item{cran}{logical (Internet needed) if the package is on CRAN, version is not taken into account here.}
#' \item{checkred}{(Optional) (Internet needed) logical if the NEWEST package contains any specified statuses on CRAN check page.}
#' \item{lifeduration}{(Optional) (Internet needed) integer number of days a package was released.}
#' }
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' The `crandb` R packages database is a part of `METACRAN` project, source:
#' Csárdi G, Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages. `https://github.com/r-hub/pkgsearch`, `https://r-hub.github.io/pkgsearch/`.
#' @export
#' @examples
#' \dontrun{
#'  lock_validate("PATH/file.lock")
#' }
lock_validate <- function(path,
                          lifeduration = FALSE,
                          checkred = list(scope = character(0), flavors = NULL),
                          repos = biocran_repos()) {
  stopifnot(file.exists(path))
  stopifnot(is.list(checkred) &&
    (length(checkred) %in% c(1, 2)) &&
    (c("scope") %in% names(checkred)) &&
    (length(checkred$scope) == 0 || all(checkred$scope %in% c("ERROR", "FAIL", "WARN", "NOTE"))) &&
    (is.null(checkred$flavors) || all(checkred$flavors %in% read_cran_flavours_raw()$Flavor)))
  stopifnot(is.character(repos))
  stopifnot(is.logical(lifeduration))

  meta_info <- jsonlite::read_json(path)
  Rv <- meta_info$R$Version
  pacs_v <- vapply(meta_info$Packages, function(x) x[["Version"]], character(1))
  pacs_n <- names(pacs_v)
  result_renv <- data.frame(Package = pacs_n, Version = pacs_v, stringsAsFactors = FALSE)
  result_renv <- rbind(result_renv, data.frame(Package = "R", Version = Rv, stringsAsFactors = FALSE))
  crandb_pacs <- crandb_json(pacs_n)

  all_data <- lapply(seq_along(pacs_n), function(x) crandb_pacs[[pacs_n[x]]]$versions[[pacs_v[x]]])
  names(all_data) <- pacs_n
  depends_pacs <- lapply(names(all_data), function(x) paste(c(names(all_data[[x]]$Depends), names(all_data[[x]]$Imports), names(all_data[[x]]$LinkingTo))))
  depends_v <- lapply(names(all_data), function(x) paste(c(all_data[[x]]$Depends, all_data[[x]]$Imports, all_data[[x]]$LinkingTo)))
  depends_df <- data.frame(Package = unlist(depends_pacs), Version = unlist(depends_v), stringsAsFactors = FALSE)
  depends_df$Version <- gsub("[ \n]", "", gsub(">=", "", gsub("\\*", "", depends_df$Version)))
  result_deps <- stats::aggregate(depends_df[, c("Version"), drop = FALSE], list(Package = depends_df$Package), pacs::compareVersionsMax)

  result <- merge(result_deps, result_renv, by = "Package", all = TRUE, suffixes = c(".expected.min", ".expected"))
  result$version_status <- vapply(
    seq_len(nrow(result)),
    function(x) utils::compareVersion(result$Version.expected[x], result$Version.expected.min[x]),
    numeric(1)
  )
  result <- result[!result$Package %in% pacs_base(), ]

  if (is_online()) {
    newest_df <- merge(
      result,
      available_packages(repos = repos)[, c("Package", "Version")],
      by = "Package",
      all.x = TRUE,
      sort = FALSE
    )

    newest_df$newest <- as.character(newest_df$Version.expected) == as.character(newest_df$Version)

    result <- merge(
      result,
      newest_df[, c("Package", "newest")],
      by = "Package",
      sort = FALSE,
      all.x = TRUE
    )

    cran_df <- merge(result[, c("Package", "Version.expected")],
      available_packages(repos = "https://cloud.r-project.org")[, c("Package", "Version")],
      by = "Package",
      all.x = TRUE,
      sort = FALSE
    )

    cran_df$cran <- !is.na(cran_df$Version)
    cran_df$cran[is.na(cran_df$cran)] <- FALSE

    result <- merge(
      result,
      cran_df[, c("Package", "cran")],
      by = "Package",
      sort = FALSE,
      all.x = TRUE
    )

    if (length(checkred$scope)) {
      checkred_all <- checked_packages()
      if (is.data.frame(checkred_all)) {
        flavors <- if (is.null(checkred$flavors)) grep("r-", colnames(checkred_all)) else checkred$flavors
        scope_final <- c(checkred$scope, paste0(checkred$scope, "*"))
        checkred_all$red_status <- apply(checkred_all[, flavors, drop = FALSE], 1, function(x) any(x %in% scope_final))
        checkred_names_scope <- checkred_all$Package[checkred_all$red_status]
        result$checkred <- (result$Package %in% checkred_names_scope) & result$newest
        result$checkred[is.na(result$newest) | !result$cran] <- NA
      } else {
        message("Failed to retrieve packages statuses with the checked_packages() function.")
        result$checkred <- NA
      }
    }

    if (lifeduration && (nrow(result) >= 500)) {
      message("Please wait, Packages life durations are assessed.\n")
      result$lifeduration <- vapply(
        seq_len(nrow(result)),
        function(x) {
          if (!isNA(version_p <- as.character(result[x, "Version.expected", drop = TRUE]))) {
            pac_lifeduration(result[x, "Package", drop = TRUE],
              version_p,
              repos = repos,
              lib.loc = .libPaths(),
              source = "cran"
            )
          } else {
            NA
          }
        }, numeric(1)
      )
    } else if (lifeduration) {
      ld <- get_crandb_lifedurations(result$Package, result$Version.expected)
      result <- merge(result, ld, by = "Package", all.x = TRUE)
    }

    not_installed <- is.na(result$Version.expected)
    if (any(not_installed)) {
      result[not_installed, intersect(c("newest", "checkred"), colnames(result))] <- NA
    }

    result
  } else {
    warning("There is no Internet connection.")
  }
}
