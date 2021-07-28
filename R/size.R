#' Size of the package
#' @description size of package.
#' @param pac character a package name.
#' @param lib.loc character vector. Default: NULL
#' @return numeric size in bytes, to get MB ten divide by `10**6`.
#' @export
#' @examples
#' cat(pacs::pacs_size("stats") / 10**6, "MB")
pac_size <- function(pac, lib.loc = NULL) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(pac %in% rownames(utils::installed.packages(lib.loc = lib.loc)))
  found <- try(find.package(pac, lib.loc = lib.loc), silent = TRUE)
  if (inherits(found, "try-error")) {
    0
  } else {
    dir_size(found)
  }
}

#' Size of packages.
#' @description size of packages.
#' @param pacs character vector packages.
#' @param lib.loc character vector. Default: NULL
#' @return numeric vector sizes in bytes, to get MB ten divide by `10**6`.
#' @export
#' @examples
#' cat(pacs::pacs_size("stats") / 10**6, "MB")
pacs_size <- function(pacs = NULL, lib.loc = NULL) {
  stopifnot(is.null(pacs) || is.character(pacs))
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))

  if (!is.null(pacs)) {
    tocheck <- pacs
  } else {
    tocheck <- rownames(utils::installed.packages(lib.loc = lib.loc))
  }

  dirs <- vapply(
    tocheck,
    function(p) pac_size(p, lib.loc = lib.loc),
    numeric(1)
  )

  stats::setNames(dirs, tocheck)
}

#' Package true size
#' @description Package true size as it takes into account its dependencies.
#' @param pac character a package name.
#' @param fields character vector, Default: `c("Depends", "Imports", "LinkingTo")`
#' @param lib.loc character vector, Default: NULL
#' @param exclude_joint integer exclude packages which are dependencies of at least N other packages, not count main package dependencies. Default: 0
#' @note R base packages are not counted.
#' @return numeric size in bytes, to get MB then divide by `10**6`.
#' @export
#' @examples
#' # size in MB, with all its dependencies
#' pacs::pac_true_size("memoise") / 10**6
#' # exclude packages if at least one other package use it too
#' pacs::pac_true_size("memoise", exclude_joint = 1L) / 10**6
pac_true_size <- function(pac,
                          fields = c("Depends", "Imports", "LinkingTo"),
                          lib.loc = NULL,
                          exclude_joint = 0L) {
  stopifnot(is.null(lib.loc) || all(lib.loc %in% .libPaths()))
  stopifnot(all(fields %in% c("Depends", "Imports", "LinkingTo", "Suggests")))
  stopifnot(is.integer(exclude_joint))

  pacs_all <- pac_deps(pac, fields = fields, lib.loc = lib.loc, attr = FALSE, base = FALSE)$Package

  if (exclude_joint) {
    depsy <- stats::setNames(lapply(pacs_all, function(x) setdiff(tools::dependsOnPkgs(x), pacs_all)), pacs_all)
    pacs_all <- setdiff(pacs_all, names(Filter(function(x) length(x) > exclude_joint, depsy)))
  }

  sum(pacs_size(unique(c(pac, setdiff(pacs_all, "R"))), lib.loc = lib.loc))
}
