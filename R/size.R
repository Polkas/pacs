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

#' Size of the shiny app
#' @description The size of shiny app is a sum of dependencies packages and the app directory.
#' The app dependencies packages are checked recursively.
#' @param path path to the shiny app. Default: `"."`
#' @return numeric size in bytes, to get MB ten divide by `10**6`.
#' @export
#' @examples
#' \dontrun{
#' # Please update the path to the shiny app
#' cat(pacs::app_size(system.file("examples/04_mpg", package = "shiny")) / 10**6, "MB")
#' }
app_size <- function(path = ".") {
  stopifnot(dir.exists(path))
  # as.character for older R versions, stringAsFactors
  app_deps_recursive <- as.character(app_deps(path, recursive = TRUE)$Package)
  if (length(app_deps_recursive) > 0) {
    sum(vapply(app_deps_recursive, pac_size, numeric(1)) + dir_size(path))
  } else {
    dir_size(path)
  }
}
