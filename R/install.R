#refuse to install not healthy package
pac_install_version <- function(pac, version = NULL, fields = c("Depends", "Imports", "LinkingTo")) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(!is.null(version))
  #order form bottom to top in dependency hierarchy
  cat("Building dependency tree.\n")
  pp = pac_deps_timemachine(pac, version = version, fields = fields)
  names_pp <- names(pp)
  lapply(seq_along(pp),
         function(x) remotes::install_version(pp[x], names_pp[x],
                                              dependencies = FALSE,
                                              upgrade = "never",
                                              force = TRUE))
}

#' Package dependencies from DESCRIPTIONS files.
#' @description Package dependencies from DESCRIPTIONS files.
#' @param pac character a package name.
#' @param fields character vector with possible values c("Depends", "Imports", "LinkingTo", "Suggests"). Default: c("Depends", "Imports", "LinkingTo")
#' @param version character version of package. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @note Longer lived version is taken if 2 is available at the same date (switch time).
#' @return named vector package dependencies and their versions at the release date + 1 of main package.
#' @export
#' @examples
#' pacs::pac_deps_timemachine("miceFast", "0.6.2")
#' pacs::pac_deps_timemachine("miceFast", at = as.Date("2019-01-01"))
pac_deps_timemachine <- function(pac,
                             version = NULL,
                             at = NULL,
                             fields = c("Depends", "Imports", "LinkingTo")) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(xor(!is.null(version), !is.null(at)))

  if (is.null(version)) {
    version <- pac_description(pac, at = at)$Version
  }

  health <- pac_health(pac, version)

  if (!health && class(health) == "sure") stop("not healthy version, live less than 7 days.")

  paks_global <- NULL
  pac_v <- pac_description(pac, version = version)$Version
  at <- as.Date(pac_timemachine(pac, version = pac_v)$Released) + 1

  deps <- function(pak, at, fields) {
    pks <- pac_description(pak, at = at, local = FALSE)
    if (pak != "R" && !pak %in% paks_global && pak != pac) {
      paks_global <<- c(paks_global, stats::setNames(pak, pks$Version))
    }

    res <- NULL
    for (f in fields) {
      ff <- pks[[f]]
      if (!is.null(ff)) {
        res <- c(
          res,
          vapply(
            strsplit(trimws(strsplit(ff, ",")[[1]]), "[ \n\\(]"),
            function(x) x[1],
            character(1)
          )
        )
      }
    }
    if (is.null(res)) {
      return(NULL)
    }

    res <- setdiff(res, pacs_base())

    for (r in res) {
      if (r != "R" && !r %in% paks_global) {
        deps(r, at, fields)
      }
    }
  }

  deps(pac, at, fields)

  paks_global
}
