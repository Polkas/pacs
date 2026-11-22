.onLoad <- function(libname, pkgname) {
  defaults <- list(pacs.crandb_limit = 500, pacs.crandb_ntry = 3, pacs.crandb_nsleep = 0.1)
  unset <- vapply(names(defaults), function(name) is.null(getOption(name)), logical(1))
  options(defaults[unset])
}
