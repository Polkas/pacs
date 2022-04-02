#' Package dependencies
#' @description Package dependencies from DESCRIPTION files with installed or expected versions or newest released.
#' @param pac character a package name.
#' @param fields a character vector listing the types of dependencies, a subset of `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
#' Character string "all" is shorthand for that vector, character string "most" for the same vector without "Enhances", character string "strong" (default) for the first three elements of that vector.
#' Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lib.loc character vector, used optionally when local is equal TRUE. Default: `.libPaths()`
#' @param base logical if to add base packages too. If `TRUE` then `pacs::pacs_base()` are taken into account. Default: FALSE
#' @param local logical if to use local repository or newest CRAN packages, where by default local packages are used. Default: TRUE
#' @param description_v if the dependencies version should be taken from description files, minimal required. By default installed versions are taken. Default: FALSE
#' @param attr logical specify if a package and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: TRUE
#' @param recursive logical If to assess the dependencies recursively. Default: TRUE
#' @param repos character vector URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' @return data.frame with packages and their versions. Versions are taken from `installed.packages` or newest released.
#' @note When function is invoked in the loop afterwards results could be aggregated like,
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
                     lib.loc = .libPaths(),
                     base = FALSE,
                     local = TRUE,
                     description_v = FALSE,
                     attr = TRUE,
                     recursive = TRUE,
                     repos = biocran_repos()) {
  fields <- expand_dependency(fields)
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.logical(base))
  stopifnot(is.logical(attr))
  stopifnot(is.logical(description_v))
  stopifnot(is.logical(recursive))
  stopifnot(is.character(repos))
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(is.logical(local))

  if (local) {
    stopifnot(pac %in% c(rownames(installed_packages(lib.loc = lib.loc)), pacs_base()))

    paks_global <- tools::package_dependencies(
      pac,
      db = installed_packages(lib.loc = lib.loc),
      which = fields,
      recursive = recursive
    )[[1]]
    pac_v <- pac_description(pac, local = TRUE, lib.loc = lib.loc)$Version

    v_base <- installed_agg_fun(lib.loc, fields)
  } else {
    if (!is_online() || isFALSE(pac_isin(pac, repos))) {
      return(NA)
    }
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
    res_df <- rbind(
      data.frame(Package = pac, Version = pac_v, stringsAsFactors = FALSE),
      res_df
    )
    res_df <- res_df[order(res_df$Package), ]
    lack_packages <- setdiff(res, res_df$Package)
    res_df_f <- res_df[res_df$Package %in% res, ]
  } else {
    lack_packages <- setdiff(res, v_base[, "Package"])
    res_df_f <- as.data.frame(v_base[v_base[, "Package"] %in% res, c("Package", "Version"), drop = FALSE])
  }

  if (length(lack_packages) > 0) {
    res_df_f <- rbind(
      res_df_f,
      data.frame(Package = lack_packages, Version = NA, stringsAsFactors = FALSE)
    )
  }

  if (attr) {
    attr(res_df_f, "Package") <- pac
    attr(res_df_f, "Version") <- pac_v
  }

  rownames(res_df_f) <- NULL
  res_df_f
}

#' Package dependencies - user perspective
#' @description A higher-level function, build from `pacs::pacs_deps`.
#' Package dependencies installed when run `installed.packages`.
#' `"Depends", "Imports", "LinkingTo"` fields from the DESCRIPTION file and
#'  their recursive dependencies taken from the same fields.
#'  Dependencies are taken remotely for the newest version.
#' @param pac character a package name.
#' @param base logical if to add base packages too.
#' @param attr logical specify if a package and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: TRUE
#' @param repos character vector URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' If `TRUE` then `pacs::pacs_base()` are taken into account. Default: FALSE
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_deps_user("dplyr")
#' pacs::pac_deps_user("cat2cat")
#' }
pac_deps_user <- function(pac, base = FALSE, attr = TRUE, repos = pacs::biocran_repos()) {
  pac_deps(pac, recursive = TRUE, description_v = TRUE, local = FALSE, base = base, attr = attr, repos = repos)
}

#' Package dependencies - developer perspective
#' @description A higher-level function, build from `pacs::pacs_deps`.
#' Package dependencies installed when checking the package.
#' `"Depends", "Imports", "LinkingTo", "Suggests"` fields from the DESCRIPTION file and
#'  their recursive dependencies taken from `"Depends", "Imports", "LinkingTo"` fields.
#'  Dependencies are taken remotely for the newest version.
#' @param pac character a package name.
#' @param base logical if to add base packages too.
#' @param attr logical specify if a package and its version should be added as a attribute of data.frame or for FALSE as a additional record. Default: TRUE
#' @param repos character vector URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' If `TRUE` then `pacs::pacs_base()` are taken into account. Default: FALSE
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_deps_dev("dplyr")
#' pacs::pac_deps_dev("cat2cat")
#' }
pac_deps_dev <- function(pac, base = FALSE, attr = TRUE, repos = pacs::biocran_repos()) {
  top <- pac_deps(pac, recursive = TRUE, description_v = TRUE, local = FALSE, base = base, attr = attr, repos = repos)
  suggs <- pac_deps(pac, recursive = FALSE, description_v = TRUE, local = FALSE, fields = "Suggests", base = base, repos = repos)
  suggs_r <- do.call(rbind, lapply(suggs$Package, function(x) pac_deps(x, description_v = TRUE, local = FALSE, base = base, repos = repos)))
  results <- do.call(rbind, list(top, suggs, suggs_r))
  stats::aggregate(results[, c("Version"), drop = FALSE], list(Package = results$Package), pacs::compareVersionsMax)
}

#' The shiny app dependencies
#' @description the shiny app dependencies packages are checked recursively.
#' The `c("Depends", "Imports", "LinkingTo")` DESCRIPTION files fields are checked recursively.
#' The required dependencies have to be installed in the local repository.
#' The default arguments setup is recommended.
#' @param path path to the shiny app. Default: `"."`
#' @param fields a character vector listing the types of dependencies, a subset of `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
#' Character string "all" is shorthand for that vector, character string "most" for the same vector without "Enhances", character string "strong" (default) for the first three elements of that vector.
#' Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lib.loc character vector, used optionally when local is equal TRUE. Default: `.libPaths()`
#' @param local logical if to use local repository or newest CRAN packages, where by default local packages are used. Default: TRUE
#' @param description_v if the dependencies version should be taken from description files, minimal required. By default installed versions are taken. Default: FALSE
#' @param recursive logical if to assess the dependencies recursively. Default: TRUE
#' @param repos character vector URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' @return character vector with dependency packages or data.frame when checking recursively.
#' @note `renv` package has to be installed. `base` packages are not taken into account.
#' @export
#' @examples
#' \dontrun{
#' library(renv)
#' # Please update the path to the custom shiny app
#' app_path <- system.file("examples/04_mpg", package = "shiny")
#' pacs::app_deps(app_path)
#' pacs::app_deps(app_path, recursive = FALSE)
#' }
app_deps <- function(path = ".",
                     fields = c("Depends", "Imports", "LinkingTo"),
                     lib.loc = .libPaths(),
                     local = TRUE,
                     description_v = FALSE,
                     recursive = TRUE,
                     repos = biocran_repos()) {
  if (requireNamespace("renv", quietly = TRUE)) {
    fields <- expand_dependency(fields)
    stopifnot(dir.exists(path))
    stopifnot(is.logical(local))
    stopifnot(is.logical(recursive))
    stopifnot(is.character(repos))
    stopifnot(is.logical(description_v))
    stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))

    app_deps <- setdiff(renv::dependencies(path, progress = FALSE)$Package, c(pacs_base(), "R"))
    if (length(app_deps) == 0) {
      return(data.frame(Package = NA, Version = NA, Direct = NA)[0, ])
    }
    not_installed <- setdiff(app_deps, rownames(installed_packages(lib.loc = .libPaths())))
    if (length(not_installed) && local) {
      stop(sprintf("Some of the dependency packages are not installed, %s", paste(not_installed, collapse = "; ")))
    }
    if (recursive) {
      app_deps_all <- lapply(app_deps, function(x) pac_deps(x, repos = repos, lib.loc = lib.loc, local = local, fields = fields, description_v = description_v, attr = FALSE))
      if (any(is.na(app_deps_all))) {
        return(NA)
      }
      app_deps_recursive <- do.call(rbind, app_deps_all)
      app_deps_recursive <- stats::aggregate(app_deps_recursive[, c("Version"), drop = FALSE], list(Package = app_deps_recursive$Package), pacs::compareVersionsMax)
      app_deps_recursive$Package <- as.character(app_deps_recursive$Package)
      app_deps_recursive$Direct <- app_deps_recursive$Package %in% app_deps
      return(app_deps_recursive)
    } else {
      return(data.frame(Package = app_deps, Version = "", Direct = TRUE, stringsAsFactors = FALSE))
    }
  } else {
    stop("Please install renv package to use app_deps or app_size functions.")
  }
}
