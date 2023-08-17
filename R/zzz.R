.onLoad <- function(libname, pkgname) {
  options(pacs.crandb_limit = 500)
  options(pacs.crandb_ntry = 3)
  options(pacs.crandb_nsleep = 0.1)
}
