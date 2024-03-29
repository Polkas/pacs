#' Size of the package
#' @description Size of package.
#' @inheritParams standard_args
#' @return `numeric` size in bytes, to get MB ten divide by `10**6`.
#' @export
#' @examples
#' \dontrun{
#' cat(pacs::pac_size("stats") / 10**6, "MB")
#' }
pac_size <- function(pac, lib.loc = .libPaths()) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
  dir_size(find.package(pac, lib.loc = lib.loc))
}

#' True size of the package
#' @description True size of the package as it takes into account its all dependencies, recursively.
#' @inheritParams standard_args
#' @note R base packages are not counted. The default value of `fields` should be suited for almost all scenarios.
#' @return `numeric` size in bytes, to get MB then divide by `10**6`.
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
  fields <- expand_dependency(fields)
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
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
#' The app dependencies packages are checked recursively, and only in local repository.
#' The default arguments setup is recommended.
#' @inheritParams standard_args
#' @return `numeric` size in bytes, to get MB ten divide by `10**6`.
#' @note `renv` package has to be installed. `base` packages (`pacs::pacs_base()`) are not taken into account.
#' @export
#' @examples
#' \dontrun{
#' library(renv)
#' # Please update the path to the shiny app
#' cat(pacs::app_size(system.file("examples/04_mpg", package = "shiny")) / 10**6, "MB")
#' }
app_size <- function(path = ".",
                     fields = c("Depends", "Imports", "LinkingTo"),
                     lib.loc = .libPaths(),
                     recursive = TRUE) {
  fields <- expand_dependency(fields)
  stopifnot(dir.exists(path))
  stopifnot(is.logical(recursive))
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))

  # as.character for older R versions, stringAsFactors
  app_deps_recursive <- as.character(app_deps(path, fields = fields, lib.loc = lib.loc, local = TRUE, recursive = recursive)$Package)
  if (length(app_deps_recursive) > 0) {
    sum(vapply(app_deps_recursive, pac_size, numeric(1)) + dir_size(path))
  } else {
    dir_size(path)
  }
}
