#' Validate the local library
#' @description
#' Checking if installed packages have correct versions taking into account all DESCRIPTION files requirements.
#' Moreover identifying which packages are newest releases.
#' Optionally we could add life duration and CRAN check page status for each package.
#' @param lib.loc character. Default: NULL
#' @param fields character vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lifeduration logical if to add life duration column, might take some time. Default: FALSE
#' @param checkred character vector scope of R CRAN check pages statuses to consider, any of `c("ERROR", "FAIL", "WARN", "NOTE")`. Default `character(0)`
#' @param repos character the base URL of the repositories to use. Default `https://cran.rstudio.com/`.
#' @return data.frame with 5/6/7 columns.
#' \describe{
#' \item{Package}{character package names.}
#' \item{Version.expected.min}{character expected by DESCRIPTION files minimal version. "" means not specified so the newest version.}
#' \item{Version.have}{character installed package version.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' \item{newest}{ logical if the installed version is the newest one.}
#' \item{checkred}{(Optional) logical if the NEWEST package contains any specified statuses on CRAN check page. `pacs::checked_packages` is used to quickly retrieve all statuses at once.}
#' \item{life_duration}{(Optional) integer number of days package was released.}
#' }
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' When turn on the `lifeduration` options, calculations might be time consuming.
#' Results are cached for 1 hour with `memoise` package.
#' @export
#' @examples
#' lib_validate()
#' \dontrun{
#' lib_validate(checkred = c("ERROR", "FAIL"))
#' lib_validate(lifeduration = TRUE, checkred = c("ERROR", "FAIL"))
#' }
lib_validate <- function(lib.loc = NULL,
                         fields = c("Depends", "Imports", "LinkingTo"),
                         lifeduration = FALSE,
                         checkred = character(0),
                         repos = "https://cran.rstudio.com/") {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(is.logical(lifeduration))
  stopifnot(length(checkred) == 0 || all(checkred %in% c("ERROR", "FAIL", "WARN", "NOTE")))

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

  newest_df <- merge(available_packages(repos = repos)[, c("Package", "Version")],
                     installed_agg[, c("Package", "Version")],
        by = "Package",
        sort = FALSE
  )

  newest_df$newest <- as.character(newest_df$Version.x) == as.character(newest_df$Version.y)

  result <- merge(result,
                  newest_df[, c("Package", "newest")],
                  by = "Package",
                  sort = FALSE,
                  all.x = TRUE)


  if (length(checkred)) {
    checkred_all <- checked_packages()
    checkred_names_scope <- checkred_all$Package[grepl(sprintf("(?:%s)", paste(checkred, collapse = "|")), checkred_all$unique_status)]
    result$checkred <- (result$Package %in% checkred_names_scope) & result$newest
  }

  if (lifeduration) {
    cat("Please wait, Packages life durations are assessed.\n")
    result$life_duration <- vapply(seq_len(nrow(result)), function(x) pac_lifeduration(result[x, "Package", drop = TRUE], as.character(result[x, "Version.have", drop = TRUE]), repos = repos, lib.loc = lib.loc), numeric(1))
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
#' @param checkred character vector scope of R CRAN check pages statuses to consider, any of `c("ERROR", "FAIL", "WARN", "NOTE")`. Default `character(0)`
#' @param repos character the base URL of the repositories to use. Default `https://cran.rstudio.com/`
#' @return data.frame with 5/6/7 columns.
#' \describe{
#' \item{Package}{character package names.}
#' \item{Version.expected.min}{character expected by DESCRIPTION files minimal version. "" means not specified so the newest version.}
#' \item{Version.have}{character installed package version.}
#' \item{version_status}{ numeric -1/0/1 which comes from `utils::compareVersion` function.
#' 0 means that we have the same version as required by DESCRIPTION files. -1 means we have too low version installed, this is an error. 1 means we have higher version.}
#' \item{newest}{ logical if the installed version is the newest one.}
#' \item{checkred}{(Optional) logical if the NEWEST package contains any specified statuses on CRAN check page.}
#' \item{life_duration}{(Optional) integer number of days package was released.}
#' }
#' @note Version.expected.min column not count packages which are not a dependency for any package, so could not be find in DESCRIPTION files.
#' When turn on the `lifeduration` option, calculations might be time consuming.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function with `lifeduration` or `checkred` turned on.
#' Results are cached with `memoise` package, memory cache.
#' @export
#' @examples
#' pac_validate("memoise")
#' pac_validate("memoise", lifeduration = TRUE, checkred = c("ERROR", "FAIL"))
pac_validate <- function(pac,
                         lib.loc = NULL,
                         fields = c("Depends", "Imports", "LinkingTo"),
                         lifeduration = FALSE,
                         checkred = character(0),
                         repos = "https://cran.rstudio.com/") {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(length(checkred) == 0 || all(checkred %in% c("ERROR", "FAIL", "WARN", "NOTE")))
  stopifnot(is.logical(lifeduration))

  descriptions_pac <- pac_deps(pac, lib.loc = lib.loc, fields = fields, description_v = TRUE)
  installed_pac <- pac_deps(pac, lib.loc = lib.loc, fields = fields)

  result <- merge(descriptions_pac,
    installed_pac,
    by = "Package",
    all = TRUE,
    suffix = c(".expected.min", ".have")
  )

  result$version_status <- apply(result, 1, function(x) utils::compareVersion(x["Version.have"], x["Version.expected.min"]))

  result <- result[!is.na(result$Package) & !(result$Package %in% c("NA", pacs_base())), ]

  result$newest <- apply(result, 1, function(x) is_last_release(x["Package"], x["Version.have"]))

  if (length(checkred)) {
    result$checkred <- vapply(seq_len(nrow(result)), function(x) isTRUE(result$newest[x] && pac_checkred(result$Package[x], scope = checkred, repos = repos)), logical(1))
  }

  if (lifeduration) {
    result$life_duration <- vapply(seq_len(nrow(result)), function(x) pac_lifeduration(result[x, "Package", drop = TRUE], as.character(result[x, "Version.have", drop = TRUE]), repos = repos, lib.loc = lib.loc), numeric(1))
  }

  result
}
