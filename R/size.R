#' Size of the package
#' @description size of package.
#' @param pac character a package name.
#' @param lib.loc character vector. Default: `.libPaths()`
#' @return numeric size in bytes, to get MB ten divide by `10**6`.
#' @export
#' @examples
#' \dontrun{
#' cat(pacs::pac_size("stats") / 10**6, "MB")
#' }
pac_size <- function(pac, lib.loc = .libPaths()) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
  found <- try(find.package(pac, lib.loc = lib.loc), silent = TRUE)
  if (inherits(found, "try-error")) {
    0
  } else {
    dir_size(found)
  }
}

#' True size of the package
#' @description True size of the package as it takes into account its dependencies.
#' @param pac character a package name.
#' @param fields character vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lib.loc character vector. Default: `.libPaths()`
#' @param exclude_joint integer exclude packages which are dependencies of at least N other packages, not count main package dependencies. Default: 0
#' @note R base packages are not counted.
#' @return numeric size in bytes, to get MB then divide by `10**6`.
#' @export
#' @examples
#' \dontrun{
#' # size in MB, with all its dependencies
#' pacs::pac_true_size("memoise") / 10**6
#' }
pac_true_size <- function(pac,
                          fields = c("Depends", "Imports", "LinkingTo"),
                          lib.loc = .libPaths(),
                          exclude_joint = 0L) {
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(all(fields %in% c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")))
  stopifnot(is.integer(exclude_joint))

  pacs_all <- pac_deps(pac, fields = fields, lib.loc = lib.loc, attr = FALSE, base = FALSE)$Package

  if (exclude_joint) {
    depsy <- stats::setNames(lapply(pacs_all, function(x) setdiff(tools::dependsOnPkgs(x), pacs_all)), pacs_all)
    pacs_all <- setdiff(pacs_all, names(Filter(function(x) length(x) > exclude_joint, depsy)))
  }

  sum(vapply(unique(c(pac, setdiff(pacs_all, "R"))), function(x) pac_size(x, lib.loc = lib.loc), numeric(1)))
}

#' The shiny app dependencies
#' @description the shiny app dependencies packages are checked recursively.
#' The `c("Depends", "Imports", "LinkingTo")` DESCRIPTION files fields are check recursively.
#' The required dependencies have to be installed in the local repository.
#' @param path path to the shiny app. Default: `"."`
#' @param recursive logical if to assess the dependencies recursively. Default: TRUE
#' @return character vector with dependency packages or data.frame when checking recursively.
#' @note the base packages are not taken into account.
#' @export
#' @examples
#' \dontrun{
#' # Please update the path to the custom shiny app
#' app_path <- system.file("examples/04_mpg", package = "shiny")
#' pacs::shiny_app_deps(app_path)
#' pacs::shiny_app_deps(app_path, recursive = FALSE)
#' }
shiny_app_deps <- function(path = ".", recursive = TRUE) {
  stopifnot(dir.exists(path))
  stopifnot(is.logical(recursive))
  app_deps <- setdiff(renv::dependencies(path, progress = FALSE)$Package, c(pacs_base(), "R"))
  if (length(app_deps) == 0) return(data.frame(Package = NA, Version = NA, Direct = NA)[0, ])
  not_installed <- setdiff(app_deps, rownames(installed_packages(lib.loc = .libPaths())))
  if (length(not_installed)) {
    stop(sprintf("Some of the dependency packages are not installed, %s", not_installed))
  }
  if (recursive) {
    app_deps_recursive <- do.call(rbind, lapply(app_deps, function(x) pac_deps(x, attr = FALSE)))
    app_deps_recursive$Direct <- app_deps_recursive$Package %in% app_deps
    return(app_deps_recursive)
  } else {
    return(data.frame(Package = app_deps, Version = "", Direct = TRUE))
  }
}

#' Size of the shiny app
#' @description size of the shiny app dependencies packages and the app directory.
#' The app dependencies packages are checked recursively.
#' @param path path to the shiny app. Default: `"."`
#' @return numeric size in bytes, to get MB ten divide by `10**6`.
#' @export
#' @examples
#' \dontrun{
#' # Please update the path to the shiny app
#' cat(pacs::shiny_app_size(system.file("examples/04_mpg", package = "shiny")) / 10**6, "MB")
#' }
shiny_app_size <- function(path = ".") {
  stopifnot(dir.exists(path))
  app_deps_recursive <- shiny_app_deps(path, recursive = TRUE)$Package
  if (length(app_deps_recursive) > 0) {
    sum(vapply(app_deps_recursive, pac_size, numeric(1)) + dir_size(path))
  } else {
    dir_size(path)
  }
}
