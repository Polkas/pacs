#' R CRAN package dependencies for a certain version or time point
#' @description Package dependencies from DESCRIPTION files retrieved recursively for certain version or time point.
#' @param pac character a package name.
#' @param fields character vector with possible values `c("Depends", "Imports", "LinkingTo", "Suggests")`. Default: `c("Depends", "Imports", "LinkingTo")`
#' @param version character version of package. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @param recursive logical if to assess the dependencies recursively. Default: TRUE
#' @note Longer lived version is taken if 2 is available at the same date (switch time).
#' @return named vector package dependencies and their versions at the release date of main package plus one day.
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_deps_timemachine("memoise", "0.2.1")
#' pacs::pac_deps_timemachine("memoise", at = as.Date("2021-01-01"))
#' }
pac_deps_timemachine <- function(pac,
                                 version = NULL,
                                 at = NULL,
                                 fields = c("Depends", "Imports", "LinkingTo"),
                                 recursive = TRUE) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo")))
  stopifnot(xor(!is.null(version), !is.null(at)))
  stopifnot(is.logical(recursive))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))

  if (is.null(version)) {
    health <- pac_health(pac, at = at)
    if (!isTRUE(health)) stop("not healthy version, live less than 14 days.")
    pac_d <- pac_description(pac, at = at, local = FALSE)
    pac_v <- pac_d$Version
  } else {
    health <- pac_health(pac, version = version)
    if (!isTRUE(health)) stop("not healthy version, live less than 14 days.")
    pac_d <- pac_description(pac, version = version, local = FALSE)
    pac_v <- pac_d$Version
    at <- as.Date(pac_timemachine(pac, version = pac_v)$Released) + 1
  }

  paks_global <- NULL

  deps <- function(pak, at, fields_real) {
    ff <- paste(unlist(fields_real), collapse = ", ")
    fff <- strsplit(trimws(strsplit(ff, ",")[[1]]), "[ \n\\(]")
    res <- NULL
    if (length(fff) > 0) {
      res <- vapply(
        fff,
        function(x) x[1],
        character(1)
      )
    }
    if (is.null(res)) {
      return(NULL)
    }

    res <- setdiff(res, pacs_base())

    for (r in res) {
      # not care about many versions for certain apckage as we taking the newset version
      if (isTRUE(r != "R" && r != pac && (!r %in% paks_global))) {
        pks <- pac_description(r, at = at, local = FALSE)
        paks_global <<- c(stats::setNames(r, pks$Version), paks_global[paks_global != r])
        if (recursive) deps(r, at, pks[fields])
      }
    }
  }

  deps(pac, at, pac_d[fields])

  paks_global
}
