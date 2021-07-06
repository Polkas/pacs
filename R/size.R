#' size of the package
#' @description size of package.
#' @param pac character a package name.
#' @param lib.loc character vector. Default: NULL
#' @return numeric size in bytes.
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
#' @return named vector of sizes in bytes.
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
#' @param base logical if to add base packages too. Default: FALSE
#' @param exclude_joint integer exclude packages which are dependencies of at least N other packages. Default: 0
#' @return numeric sizes in bytes.
#' @export
#' @examples
#' # size in Mb
#' pacs::pac_true_size("stats") / 10**6
#' # exclude packages if at least one other package use it too
#' \dontrun{
#' pacs::pac_true_size("devtools", exclude_joint = 1L)/10**6
#' }
pac_true_size <- function(pac,
                          fields = c("Depends", "Imports", "LinkingTo"),
                          lib.loc = NULL,
                          base = FALSE,
                          exclude_joint = 0L) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "LinkingTo", "Suggests")))
  stopifnot(is.integer(exclude_joint))

  pacs_all <- pac_deps(pac, fields = fields, lib.loc = lib.loc, attr = FALSE, base = base)$Package

  if (exclude_joint) {
    depsy <- stats::setNames(lapply(pacs_all, function(x) tools::dependsOnPkgs(x)), pacs_all)
    pacs_all <- setdiff(pacs_all, names(Filter(function(x) length(x) > exclude_joint, depsy)))
  }

  sum(pacs_size(setdiff(pacs_all, "R"), lib.loc = lib.loc))

}

#' Shiny app true size
#' @description shiny app true size as it takes into account all its dependencies.
#' @param path character path to shiny app. Default: getwd()
#' @return numeric sizes in bytes.
#' @export
shiny_true_size <- function(path = getwd()) {
  appDependencies <- utils::getFromNamespace("appDependencies", "packrat")
  deps <- appDependencies(path)
  app_size <- dir_size(path)
  (sum(pacs::pacs_size(deps)) + app_size)
}
