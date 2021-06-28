
replaceNA <- function(vec, with) {
  vec[is.na(vec)] <- with
  vec
}

#' Maximum version across vector
#' @description Reduce function over the utils::compareVersion
#' @param vec character vector
#' @param na.rm logical if to remove NA values.
#' @return character
#' @export
#'

compareVersionsMax <- function(vec, na.rm = TRUE) {
  stopifnot(is.logical(na.rm))
  if (length(vec) == 1) return(vec)
  if (na.rm) vec <- stats::na.omit(vec)
  Reduce(function(x, y) {
    cc <- utils::compareVersion(x, y)
    if (cc == 1) {
      x
    } else if (cc == -1) {
      y
    } else {
      x
    }
  },
  vec)
}

#' Minimum version across vector
#' @description Reduce function over the utils::compareVersion
#' @param vec character vector
#' @param na.rm logical if to remove NA values.
#' @return character
#' @export
#'

compareVersionsMin <- function(vec, na.rm = TRUE) {
  stopifnot(is.logical(na.rm))
  if (length(vec) == 1) return(vec)
  if (na.rm) vec <- stats::na.omit(vec)
  Reduce(function(x, y) {
    cc <- utils::compareVersion(x, y)
    if (cc == 1) {
      y
    } else if (cc == -1) {
      x
    } else {
      x
    }
  },
  vec)
}


dir_size <- function(path, recursive = TRUE) {
  stopifnot(is.character(path))
  files <- list.files(path, full.names = T, recursive = recursive)
  vect_size <- sapply(files, function(x) file.size(x))
  size_files <- sum(vect_size)
  size_files
}

is_online <- function(site = "http://example.com/") {
  tryCatch({
    suppressWarnings(readLines(site, n = 1))
    TRUE
  },
  error = function(e) FALSE)
}
