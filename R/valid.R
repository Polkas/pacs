#' Validate the local library
#' @description
#' Checking if installed packages have correct versions taking into account all DESCRIPTION files requirements.
#' Moreover identifying which packages are newest releases.
#' Optionally we could add life duration and CRAN check page status for each package.
#' @inheritParams standard_args
#' @return `data.frame` with 4/6/8/9/10 columns.
#' \describe{
#' \item{Package}{character a package name.}
#' \item{Version.expected.min}{character expected by DESCRIPTION files minimal version. "" means not specified so the newest version.}
#' \item{Version.have}{character installed package version.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' \item{built}{character package was built under this R version}
#' \item{built_status}{integer if the package was built under the current R version, then 1 (good) and for older R versions 0 (possibly bad).
#' A package built under older R version or mix of packages built under different versions could bring possible failures.}
#' \item{newest}{logical (Internet needed) if the installed version is the newest one. For Bioconductor if is the newest one per R version.}
#' \item{cran}{logical (Internet needed) if the package is on CRAN, version is not taken into account here.}
#' \item{checkred}{(Optional) (Internet needed) logical if the NEWEST package contains any specified statuses on CRAN check page. `pacs::checked_packages` is used to quickly retrieve all statuses at once.}
#' \item{lifeduration}{(Optional) (Internet needed) integer number of days a package was released.}
#' }
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' When turn on the `lifeduration` options, calculations might be time consuming for libraries bigger than 500 packages.
#' Results are cached for 30 minutes with `memoise` package.
#' `BioConductor` packages are tested only in available scope, `checkred` is not assessed for them.
#' The `crandb` R packages database is a part of `METACRAN` project, source:
#' Csárdi G, Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages. `https://github.com/r-hub/pkgsearch`, `https://r-hub.github.io/pkgsearch/`.
#' @export
#' @examples
#' \dontrun{
#' pacs::lib_validate()
#' pacs::lib_validate(checkred = list(scope = c("ERROR", "FAIL", "WARN")))
#' pacs::lib_validate(checkred = list(
#'   scope = c("ERROR", "FAIL"),
#'   flavors = pacs::match_flavors()
#' ))
#' # activate lifeduration argument, could be time consuming for bigger libraries.
#' pacs::lib_validate(
#'   lifeduration = TRUE,
#'   checkred = list(scope = c("ERROR", "FAIL"))
#' )
#' # only R CRAN repository
#' pacs::lib_validate(repos = "https://cran.rstudio.com/")
#' }
lib_validate <- function(lib.loc = .libPaths(),
                         fields = c("Depends", "Imports", "LinkingTo"),
                         lifeduration = FALSE,
                         checkred = list(scope = character(0), flavors = NULL),
                         built = FALSE,
                         repos = biocran_repos()) {
  fields <- expand_dependency(fields)
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(is.list(checkred) &&
    (length(checkred) %in% c(1, 2)) &&
    (c("scope") %in% names(checkred)) &&
    (length(checkred$scope) == 0 || all(checkred$scope %in% c("ERROR", "FAIL", "WARN", "NOTE"))) &&
    (is.null(checkred$flavors) || all(checkred$flavors %in% cran_flavors()$Flavor)))
  stopifnot(is.character(repos))
  stopifnot(is.logical(lifeduration))
  stopifnot(is.logical(built))

  installed_agg <- installed_agg_fun(lib.loc, "Built")
  res_agg <- installed_descriptions(lib.loc, fields)

  Rv <- paste0(R.Version()[c("major", "minor")], collapse = ".")

  result <- merge(
    res_agg,
    rbind(
      installed_agg[, c("Package", "Version", "Built")],
      data.frame(
        Package = "R",
        Version = Rv,
        Built = Rv,
        stringsAsFactors = FALSE
      )
    ),
    by = "Package",
    all = TRUE,
    suffix = c(".expected.min", ".have")
  )

  result$version_status <- apply(result, 1, function(x) utils::compareVersion(x["Version.have"], x["Version.expected.min"]))

  if (built) {
    result$built <- result$Built
    result$built_status <- as.integer(result$Built == Rv)
  }
  result$Built <- NULL

  result <- result[!is.na(result$Package) & !(result$Package %in% c("", "NA", pacs_base())), ]

  if (is_online()) {
    result <- validate_online(result, "Version.have", lifeduration, checkred, repos, lib.loc)
  } else {
    message("No internet connection detected.\n")
  }

  result
}

#' Validate a specific local package
#' @description
#' Checking if installed package dependencies have correct versions taking into account their DESCRIPTION files requirements.
#' Moreover identifying which packages are newest releases.
#' Optionally we could add life duration and CRAN check page status for each dependency.
#' @inheritParams standard_args
#' @return `data.frame` with 5/7/8/9 columns.
#' \describe{
#' \item{Package}{character a package name.}
#' \item{Version.expected.min}{character expected by DESCRIPTION files minimal version. "" means not specified so the newest version.}
#' \item{Version.have}{character installed package version.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' \item{direct}{ logical if the package is in the first dependency layer, direct dependencies from DESCRIPTION file.}
#' \item{newest}{ logical (Internet needed) if the installed version is the newest one.}
#' \item{cran}{logical (Internet needed) if the package is on CRAN, version is not taken into account here.}
#' \item{checkred}{(Optional) (Internet needed) logical if the NEWEST package contains any specified statuses on CRAN check page.}
#' \item{lifeduration}{(Optional) (Internet needed) integer number of days a package was released.}
#' }
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' When turn on the `lifeduration` option, calculations might be time consuming when there is more than 500 packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function with `lifeduration` or `checkred` turned on.
#' Results are cached with `memoise` package, memory cache.
#' The `crandb` R packages database is a part of `METACRAN` project, source:
#' Csárdi G, Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages. `https://github.com/r-hub/pkgsearch`, `https://r-hub.github.io/pkgsearch/`.
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_validate("memoise")
#' pacs::pac_validate(
#'   "memoise",
#'   lifeduration = TRUE,
#'   checkred = list(scope = c("ERROR", "FAIL"), flavors = NULL)
#' )
#' pacs::pac_validate(
#'   "memoise",
#'   lifeduration = TRUE,
#'   checkred = list(scope = c("ERROR", "FAIL"), flavors = pacs::match_flavors())
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
    (is.null(checkred$flavors) || all(checkred$flavors %in% cran_flavors()$Flavor)))
  stopifnot(is.character(repos))
  stopifnot(is.logical(lifeduration))

  descriptions_pac <- pac_deps(pac, lib.loc = lib.loc, fields = fields, description_v = TRUE)
  descriptions_pac_direct <- pac_deps(pac, lib.loc = lib.loc, fields = fields, description_v = TRUE, recursive = FALSE)

  installed_pac <- pac_deps(pac, lib.loc = lib.loc, fields = fields)
  installed_agg <- installed_agg_fun(lib.loc, "Built")

  result <- merge(
    descriptions_pac,
    installed_pac,
    by = "Package",
    all = TRUE,
    suffix = c(".expected.min", ".have")
  )

  if (nrow(result)) {
    result$version_status <- apply(result, 1, function(x) utils::compareVersion(x["Version.have"], x["Version.expected.min"]))

    result <- merge(
      result,
      installed_agg[, c("Package", "Built")],
      by = "Package"
    )

    Rv <- paste0(R.Version()[c("major", "minor")], collapse = ".")
    result$built <- result$Built
    result$built_status <- as.integer(result$Built == Rv)
    result$Built <- NULL

    result <- result[!is.na(result$Package) & !(result$Package %in% c("", "NA", pacs_base())), ]
    result$direct <- result$Package %in% descriptions_pac_direct$Package

    if (is_online()) {
      result <- validate_online(result, "Version.have", lifeduration, checkred, repos, lib.loc)
    } else {
      message("No internet connection detected.\n")
    }
  }

  result
}

#' Validate a specific renv lock file
#' @description
#' This function will be especially useful when renv lock file is built manually.
#' Checking if packages in the lock file have correct versions taking into account their DESCRIPTION files requirements (`c("Depends", "Imports", "LinkingTo")`).
#' Moreover identifying which packages are newest releases.
#' Optionally we could add life duration and CRAN check page status for each dependency.
#' @inheritParams standard_args
#' @return `data.frame` with 2/6/7/8 columns.
#' \describe{
#' \item{Package}{character a package name.}
#' \item{Version.expected.min}{(conditional) (Internet needed) character expected by DESCRIPTION files minimal version. "" means not specified so the newest version.}
#' \item{Version.expected}{character package version in the renv lock file.}
#' \item{version_status}{(conditional) numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' \item{newest}{ logical (Internet needed) if the installed version is the newest one.}
#' \item{cran}{logical (Internet needed) if the package is on CRAN, version is not taken into account here.}
#' \item{checkred}{(Optional) (Internet needed) logical if the NEWEST package contains any specified statuses on CRAN check page.}
#' \item{lifeduration}{(Optional) (Internet needed) integer number of days a package was released.}
#' }
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' `Version.expected.min` and `version_status` are assessed only if there are less than 500 packages in the lock file.
#' When turn on the `lifeduration` option, calculations might be time consuming when there is more than 500 packages.
#' The `crandb` R packages database is a part of `METACRAN` project, source:
#' Csárdi G, Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages. `https://github.com/r-hub/pkgsearch`, `https://r-hub.github.io/pkgsearch/`.
#' @export
#' @examples
#' \dontrun{
#' # path or url
#' url <- "https://raw.githubusercontent.com/Polkas/pacs/master/tests/testthat/files/renv_test.lock"
#' pacs::lock_validate(url)
#'
#' pacs::lock_validate(
#'   url,
#'   checkred = list(scope = c("ERROR", "FAIL"), flavors = pacs::match_flavors())
#' )
#'
#' pacs::lock_validate(
#'   url,
#'   lifeduration = TRUE,
#'   checkred = list(scope = c("ERROR", "FAIL"), flavors = NULL)
#' )
#' }
lock_validate <- function(path,
                          lifeduration = FALSE,
                          checkred = list(scope = character(0), flavors = NULL),
                          lib.loc = .libPaths(),
                          repos = biocran_repos()) {
  stopifnot(is.list(checkred) &&
    (length(checkred) %in% c(1, 2)) &&
    (c("scope") %in% names(checkred)) &&
    (length(checkred$scope) == 0 || all(checkred$scope %in% c("ERROR", "FAIL", "WARN", "NOTE"))) &&
    (is.null(checkred$flavors) || all(checkred$flavors %in% cran_flavors()$Flavor)))
  stopifnot(is.character(repos))
  stopifnot(is.logical(lifeduration))

  meta_info <- jsonlite::read_json(path)
  Rv <- meta_info$R$Version
  pacs_v <- vapply(meta_info$Packages, function(x) x[["Version"]], character(1))
  pacs_n <- names(pacs_v)
  result_renv <- data.frame(Package = pacs_n, Version = pacs_v, stringsAsFactors = FALSE)
  result_renv <- rbind(result_renv, data.frame(Package = "R", Version = Rv, stringsAsFactors = FALSE))
  rownames(result_renv) <- NULL

  crandb_limit_ok <- length(pacs_n) <= getOption("pacs.crandb_limit", 100)

  if (is_online()) {
    crandb_pacs <- crandb_json(pacs_n)
    if (crandb_limit_ok && !isNA(crandb_pacs)) {
      all_data <- lapply(seq_along(pacs_n), function(x) crandb_pacs[[pacs_n[x]]]$versions[[pacs_v[x]]])
      names(all_data) <- pacs_n
      depends_pacs <- lapply(names(all_data), function(x) paste(c(names(all_data[[x]]$Depends), names(all_data[[x]]$Imports), names(all_data[[x]]$LinkingTo))))
      depends_v <- lapply(names(all_data), function(x) paste(c(all_data[[x]]$Depends, all_data[[x]]$Imports, all_data[[x]]$LinkingTo)))
      depends_df <- data.frame(Package = unlist(depends_pacs), Version.expected = unlist(depends_v), stringsAsFactors = FALSE)
      depends_df$Version <- gsub("[ \n]", "", gsub(">=", "", gsub("\\*", "", depends_df$Version)))
      result_deps <- stats::aggregate(depends_df[, c("Version"), drop = FALSE], list(Package = depends_df$Package), pacs::compareVersionsMax)

      result <- merge(result_deps, result_renv, by = "Package", all = TRUE, suffixes = c(".expected.min", ".expected"))
      result$version_status <- vapply(
        seq_len(nrow(result)),
        function(x) utils::compareVersion(result$Version.expected[x], result$Version.expected.min[x]),
        numeric(1)
      )
    } else {
      if (!crandb_limit_ok) {
        message(sprintf("There is more packages than crandb limit of %s.\n", getOption("pacs.crandb_limit", 100)))
      }
      if (isNA(crandb_pacs)) {
        message("crandb fetch failed, please try again.\n")
      }
      result <- result_renv
      colnames(result) <- c("Package", "Version.expected")
    }

    result <- result[!is.na(result$Package) & !(result$Package %in% c("", "NA", pacs_base())), ]
    result <- validate_online(result, "Version.expected", lifeduration, checkred, repos)
  } else {
    message("No internet connection detected.\n")
    result <- result_renv
    colnames(result)[colnames(result) == "Version"] <- "Version.expected"
  }

  result
}

#' Append a data.frame with online attributes
#' @description internal function to append a data.frame with an online related sources.
#' The input data.frame has a specific structure, such contain `c("Package", version_name_new)` columns.
#' @keywords internal
validate_online <- function(result,
                            version_name_new = "Version.expected",
                            lifeduration,
                            checkred,
                            repos,
                            lib.loc = .libPaths()) {
  stopifnot(c("Package", version_name_new) %in% colnames(result))
  newest_df <- merge(
    result,
    available_packages(repos = repos)[, c("Package", "Version")],
    by = "Package",
    all.x = TRUE,
    sort = FALSE
  )

  version_base <- as.character(newest_df[["Version"]])
  version_rel <- as.character(newest_df[[version_name_new]])
  newest_df$newest <- vapply(
    seq_len(nrow(newest_df)),
    function(x) utils::compareVersion(version_rel[x], version_base[x]),
    numeric(1)
  ) >= 0

  result <- merge(
    result,
    newest_df[, c("Package", "newest")],
    by = "Package",
    sort = FALSE,
    all.x = TRUE
  )

  cran_df <- merge(result[, c("Package", version_name_new)],
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
      # each scope could have a star at the end
      scope_final <- c(checkred$scope, paste0(checkred$scope, "*"))
      checkred_all$red_status <- apply(checkred_all[, flavors, drop = FALSE], 1, function(x) any(x %in% scope_final))
      checkred_names_scope <- checkred_all$Package[checkred_all$red_status]
      result$checkred <- (result$Package %in% checkred_names_scope) & result$newest
      result$checkred[is.na(result$newest) | !result$cran] <- NA
    } else {
      message("Failed to retrieve packages statuses with the checked_packages() function.\n")
      result$checkred <- NA
    }
  }

  if (lifeduration) {
    message("Please wait, Packages life durations are assessed.\n")

    lifesource <- if ((nrow(result) > getOption("pacs.crandb_limit", 100))) {
      "loop_crandb"
    } else {
      "crandb"
    }

    ld <- pacs_lifeduration(
      pacs = result$Package,
      versions = result[[version_name_new]],
      source = lifesource,
      lib.loc = lib.loc,
      repos = repos
    )

    if (is.data.frame(ld)) {
      result <- merge(result, ld, by = "Package", all.x = TRUE)
    } else {
      message("Failed to fetch lifedurations.\n")
      result$lifeduration <- NA
    }
  }

  not_installed <- is.na(result[[version_name_new]])
  if (any(not_installed)) {
    result[not_installed, intersect(c("newest", "checkred"), colnames(result))] <- NA
  }

  result
}
