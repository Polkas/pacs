#' Validate the local library
#' @description
#' Checking if installed packages have correct versions taking into account all DESCRIPTION files requirements.
#' Moreover identifying which packages are newest releases.
#' Optionally we could add life duration and CRAN check page status for each package.
#' @param lib.loc character. Default: NULL
#' @param fields character vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lifeduration logical if to add life duration column, might take some time. Default: FALSE
#' @param checkred list with two named fields, `scope` and `flavor`. `scope` of R CRAN check pages statuses to consider, any of `c("ERROR", "FAIL", "WARN", "NOTE")`. `flavor` is a vector of CRAN machines to consider, which might be retrieved with `pacs::cran_flavors()$Flavor`. By default an empty scope field deactivated assessment for `checkred` column, and NULL flavor will results in checking all machines. Default `list(scope = character(0), flavor = NULL)`
#' @param repos character the base URL of the repository to use. Default `pacs::biocran_repos()`
#' @return data.frame with 6/7/8 columns.
#' \describe{
#' \item{Package}{character a package name.}
#' \item{Version.expected.min}{character expected by DESCRIPTION files minimal version. "" means not specified so the newest version.}
#' \item{Version.have}{character installed package version.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' \item{newest}{logical if the installed version is the newest one.}
#' \item{cran}{logical if the package is on CRAN, version is not taken into accout here.}
#' \item{checkred}{(Optional) logical if the NEWEST package contains any specified statuses on CRAN check page. `pacs::checked_packages` is used to quickly retrieve all statuses at once.}
#' \item{lifeduration}{(Optional) integer number of days a package was released.}
#' }
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' When turn on the `lifeduration` options, calculations might be time consuming.
#' Results are cached for 1 hour with `memoise` package.
#' `BioConductor` packages are tested only in available scope, `checkred` is not assessed for them.
#' @export
#' @examples
#' \dontrun{
#' lib_validate()
#' lib_validate(checkred = list(scope = c("ERROR", "FAIL", "WARN")))
#' lib_validate(checkred = list(scope = c("ERROR", "FAIL"),
#'              flavors = cran_flavors()$Flavor[1:2]))
#' # activate lifeduration argument, could be time consuming for bigger libraries.
#' lib_validate(lifeduration = TRUE,
#'              checkred = list(scope = c("ERROR", "FAIL")))
#' }
lib_validate <- function(lib.loc = NULL,
                         fields = c("Depends", "Imports", "LinkingTo"),
                         lifeduration = FALSE,
                         checkred = list(scope = character(0), flavors = NULL),
                         repos = biocran_repos()) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(is.logical(lifeduration))
  stopifnot(is.list(checkred) &&
              length(checkred) %in% c(1,2) &&
              (c("scope") %in% names(checkred)) &&
              length(checkred$scope) == 0 || all(checkred$scope %in% c("ERROR", "FAIL", "WARN", "NOTE")) &&
              is.null(checkred$flavors) || all(checkred$flavors %in% cran_flavors()$Flavor)
  )

  installed_agg <- installed_agg_fun(lib.loc, fields)

  res_agg <- installed_descriptions(lib.loc, fields)

  result <- merge(res_agg,
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

  newest_df <- merge(installed_agg[, c("Package", "Version")],
                     available_packages(repos = repos)[, c("Package", "Version")],
        by = "Package",
        all.x = TRUE,
        sort = FALSE
  )

  newest_df$newest <- as.character(newest_df$Version.x) == as.character(newest_df$Version.y)

  result <- merge(result,
                  newest_df[, c("Package", "newest")],
                  by = "Package",
                  sort = FALSE,
                  all.x = TRUE)

  cran_df <- merge(installed_agg[, c("Package", "Version")],
                    available_packages(repos = "https://cloud.r-project.org")[, c("Package", "Version")],
                     by = "Package",
                     all.x = TRUE,
                     sort = FALSE
  )

  cran_df$cran <- !is.na(cran_df$Version.y)
  cran_df$cran[is.na(cran_df$cran)] <- FALSE

  result <- merge(result,
                  cran_df[, c("Package", "cran")],
                  by = "Package",
                  sort = FALSE,
                  all.x = TRUE)

  if (length(checkred$scope)) {
    checkred_all <- checked_packages()
    flavors <- if (is.null(checkred$flavors)) grep("r-", colnames(checkred_all)) else checkred$flavors
    scope_final <- c(checkred$scope, paste0(checkred$scope, "*"))
    checkred_all$red_status <- apply(checkred_all[, flavors, drop = FALSE], 1, function(x) any(x %in% scope_final))
    checkred_names_scope <- checkred_all$Package[checkred_all$red_status]
    result$checkred <- (result$Package %in% checkred_names_scope) & result$newest
    result$checkred[is.na(result$newest) | !result$cran] <- NA
  }

  if (lifeduration) {
    cat("Please wait, Packages life durations are assessed.\n")
    result$lifeduration <- vapply(seq_len(nrow(result)), function(x) pac_lifeduration(result[x, "Package", drop = TRUE], as.character(result[x, "Version.have", drop = TRUE]), repos = repos, lib.loc = lib.loc), numeric(1))
  }

  result
}

#' Validate a specific local package
#' @description
#' Checking if installed package dependencies have correct versions taking into account their DESCRIPTION files requirements.
#' Moreover identifying which packages are newest releases.
#' Optionally we could add life duration and CRAN check page status for each dependency.
#' @param pac character a package name.
#' @param lib.loc character. Default: NULL
#' @param fields character vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lifeduration logical if to add life duration column, might take some time. Default: FALSE
#' @param checkred list with two named fields, `scope` and `flavor`. `scope` of R CRAN check pages statuses to consider, any of `c("ERROR", "FAIL", "WARN", "NOTE")`. `flavor` vector of machines to consider, which might be retrieved with `pacs::cran_flavors()$Flavor`. By default an empty scope field deactivated assessment for `checkred` column, and NULL flavor will results in checking all machines. Default `list(scope = character(0), flavor = NULL)`
#' @param repos character the base URL of the repository to use. Default `pacs::biocran_repos()`
#' @return data.frame with 5/6/7 columns.
#' \describe{
#' \item{Package}{character a package name.}
#' \item{Version.expected.min}{character expected by DESCRIPTION files minimal version. "" means not specified so the newest version.}
#' \item{Version.have}{character installed package version.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' \item{newest}{ logical if the installed version is the newest one.}
#' \item{cran}{logical if the package is on CRAN, version is not taken into accout here.}
#' \item{checkred}{(Optional) logical if the NEWEST package contains any specified statuses on CRAN check page.}
#' \item{lifeduration}{(Optional) integer number of days a package was released.}
#' }
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' When turn on the `lifeduration` option, calculations might be time consuming.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function with `lifeduration` or `checkred` turned on.
#' Results are cached with `memoise` package, memory cache.
#' @export
#' @examples
#' \dontrun{
#' pac_validate("memoise")
#' pac_validate("memoise",
#'              lifeduration = TRUE,
#'              checkred = list(scope = c("ERROR", "FAIL"), flavors = NULL))
#' }
pac_validate <- function(pac,
                         lib.loc = NULL,
                         fields = c("Depends", "Imports", "LinkingTo"),
                         lifeduration = FALSE,
                         checkred = list(scope = character(0), flavors = NULL),
                         repos = biocran_repos()) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.logical(lifeduration))
  stopifnot(is.list(checkred) &&
              length(checkred) %in% c(1,2) &&
              (c("scope") %in% names(checkred)) &&
              length(checkred$scope) == 0 || all(checkred$scope %in% c("ERROR", "FAIL", "WARN", "NOTE")) &&
              is.null(checkred$flavors) || all(checkred$flavors %in% cran_flavors()$Flavor)
  )

  descriptions_pac <- pac_deps(pac, lib.loc = lib.loc, fields = fields, description_v = TRUE)
  installed_pac <- pac_deps(pac, lib.loc = lib.loc, fields = fields)

  result <- merge(descriptions_pac,
    installed_pac,
    by = "Package",
    all = TRUE,
    suffix = c(".expected.min", ".have")
  )

  if (nrow(result)) {
  result$version_status <- apply(result, 1, function(x) utils::compareVersion(x["Version.have"], x["Version.expected.min"]))

  result <- result[!is.na(result$Package) & !(result$Package %in% c("NA", pacs_base())), ]

  result$newest <- apply(result, 1, function(x) isTRUE(pac_last(x["Package"]) == x["Version.have"]))

  result$cran <- apply(result, 1, function(x) isTRUE(is_cran(x["Package"])))

  if (length(checkred$scope)) {
    result$checkred <- vapply(seq_len(nrow(result)), function(x) isTRUE(result$newest[x] && result$cran[x] && pac_checkred(result$Package[x], scope = checkred$scope, flavors = checkred$flavors)), logical(1))
  }

  if (lifeduration) {
    result$lifeduration <- vapply(seq_len(nrow(result)), function(x) pac_lifeduration(result[x, "Package", drop = TRUE], as.character(result[x, "Version.have", drop = TRUE]), repos = repos, lib.loc = lib.loc), numeric(1))
  }
  }
  result
}
