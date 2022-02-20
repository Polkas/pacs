
replaceNA <- function(vec, with) {
  vec[is.na(vec)] <- with
  vec
}

isNA <- function(x) {
  isTRUE(length(x) == 1 && is.na(x))
}

#' Maximum version across the vector
#' @description Reduce function over the `utils::compareVersion`
#' @param vec character vector
#' @param na.rm logical if to remove NA values.
#' @return character maximum version
#' @examples
#' compareVersionsMax(c("1.1.1", "0.2.0"))
#' @export
#'
compareVersionsMax <- function(vec, na.rm = TRUE) {
  stopifnot(is.logical(na.rm))
  if (length(vec) == 1) {
    return(vec)
  }
  if (na.rm) vec <- stats::na.omit(vec)
  Reduce(
    function(x, y) {
      cc <- utils::compareVersion(x, y)
      if (cc == 1) {
        x
      } else if (cc == -1) {
        y
      } else {
        x
      }
    },
    vec
  )
}

#' Minimum version across the vector
#' @description Reduce function over the `utils::compareVersion`
#' @param vec character vector
#' @param na.rm logical if to remove NA values.
#' @return character minimal version
#' @examples
#' compareVersionsMin(c("1.1.1", "0.2.0"))
#' @export
#'
compareVersionsMin <- function(vec, na.rm = TRUE) {
  stopifnot(is.logical(na.rm))
  if (length(vec) == 1) {
    return(vec)
  }
  if (na.rm) vec <- stats::na.omit(vec)
  Reduce(
    function(x, y) {
      cc <- utils::compareVersion(x, y)
      if (cc == 1) {
        y
      } else if (cc == -1) {
        x
      } else {
        x
      }
    },
    vec
  )
}

#' Size of the package
#' @description size of package.
#' @param path path to the directory. Default: `"."`
#' @param recursive logical if to assess the dependencies recursively. Default: TRUE
#' @return numeric size in bytes, to get MB ten divide by `10**6`.
#' @export
#' @examples
#' \dontrun{
#' cat(pacs::dir_size(system.file(package = "stats")) / 10**6, "MB")
#' }
dir_size <- function(path = ".", recursive = TRUE) {
  stopifnot(is.character(path))
  stopifnot(is.logical(recursive))
  files <- list.files(path, full.names = T, recursive = recursive)
  vect_size <- sapply(files, function(x) file.size(x))
  size_files <- sum(vect_size)
  size_files
}

is_online <- function(site = "https://example.com/") {
  tryCatch(
    {
      suppressWarnings(readLines(site, n = 1, warn = FALSE))
      TRUE
    },
    error = function(e) FALSE
  )
}

#' List of base R packages
#' @description using installed.packages and priority equal "base" to retrieve base packages.
#' @param startup logical include only startup packages. Default: FALSE
#' @return character vector
#' @examples
#' \dontrun{
#' pacs_base()
#' pacs_base(startup = TRUE)
#' }
#' @export
#'
pacs_base <- function(startup = FALSE) {
  stopifnot(is.logical(startup))
  if (startup) {
    c(getOption("defaultPackages"), "base")
  } else {
    pacs_base_all()
  }
}

pacs_base_all_raw <- function() {
  rownames(installed_packages(lib.loc = NULL, priority = "base"))
}

pacs_base_all <- memoise::memoise(pacs_base_all_raw)

installed_descriptions <- function(lib.loc, fields, deps = NULL) {
  installed_agg <- installed_agg_fun(lib.loc, fields)

  paks <- installed_agg[, fields]

  if (!is.null(deps)) {
    nams <- installed_agg[, c("Package")]
    paks <- paks[nams %in% deps, ]
  }

  joint_cols <- apply(paks, 1, function(x) paste(x, collapse = ","))

  desc_e <- extract_deps(joint_cols)

  packages <- desc_e$packages
  versions <- desc_e$versions

  joint <- data.frame(
    Version = unlist(lapply(seq_along(packages), function(x) replaceNA(versions[[x]], ""))),
    Package = unlist(lapply(seq_along(packages), function(x) replace(packages[[x]], packages[[x]] == "NA", NA))),
    stringsAsFactors = FALSE
  )

  res_agg <- stats::aggregate(
    joint[, c("Version"), drop = FALSE],
    list(Package = joint$Package),
    compareVersionsMax
  )

  res_agg$Version[is.na(res_agg$Version)] <- ""

  res_agg[!is.na(res_agg$Package), ]
}

installed_agg_fun_raw <- function(lib.loc, fields) {
  installed_df <- as.data.frame(installed_packages(lib.loc = lib.loc))
  installed_agg <- stats::aggregate(
    installed_df[, c("Version", fields), drop = FALSE],
    list(Package = installed_df$Package),
    function(x) x[1]
  )
  installed_agg
}

installed_agg_fun <- memoise::memoise(installed_agg_fun_raw, cache = cachem::cache_mem(max_age = 15 * 60))

#' List Available Packages at CRAN-like Repositories
#' @description available_packages returns a matrix of details corresponding to packages currently available at one or more repositories. The current list of packages is downloaded over the internet (or copied from a local mirror).
#' @param repos character vector, the base URL(s) of the repositories to use. Default `pacs::biocran_repos()`
available_packages <- function(repos) {
  tryCatch(available_packages_raw(repos = repos), error = function(e) NA)
}

available_packages_raw <- memoise::memoise(utils::available.packages, cache = cachem::cache_mem(max_age = 15 * 60))

installed_packages <- function(lib.loc, priority = NULL) {
  installed_packages_raw(lib.loc = lib.loc, priority = priority)
}

installed_packages_raw <- memoise::memoise(utils::installed.packages, cache = cachem::cache_mem(max_age = 15 * 60))

extract_deps <- function(x) {
  splited <- stri_split_fixed(x, ",")
  trimed <- lapply(splited, stri_trim)
  v_reg <- function(x) {
    vapply(
      stringi::stri_match_all(x, regex = "([0-9\\.-]+)\\)"),
      function(i) `[`(i, 2),
      character(1)
    )
  }
  versions <- lapply(trimed, v_reg)
  v_pac <- function(x) {
    vapply(
      stri_split_regex(x, "[ \n\\(]"),
      function(x) `[[`(x, 1),
      character(1)
    )
  }
  pacs <- lapply(trimed, v_pac)
  list(packages = pacs, versions = versions)
}

available_descriptions <- function(repos, fields, deps = NULL) {
  available_agg <- available_agg_fun(repos, fields)

  paks <- available_agg[, fields]

  if (!is.null(deps)) {
    nams <- available_agg[, c("Package")]
    paks <- paks[nams %in% deps, ]
  }

  joint_cols <- apply(paks, 1, function(x) paste(x, collapse = ","))

  desc_e <- extract_deps(joint_cols)

  packages <- desc_e$packages
  versions <- desc_e$versions

  joint <- data.frame(
    Version = unlist(sapply(seq_along(packages), function(x) replaceNA(versions[[x]], ""))),
    Package = unlist(sapply(seq_along(packages), function(x) replace(packages[[x]], packages[[x]] == "NA", NA))),
    stringsAsFactors = FALSE
  )

  res_agg <- stats::aggregate(
    joint[, c("Version"), drop = FALSE],
    list(Package = joint$Package),
    compareVersionsMax
  )

  res_agg$Version[is.na(res_agg$Version)] <- ""

  res_agg
}

available_agg_fun_raw <- function(repos, fields) {
  available_df <- as.data.frame(available_packages(repos = repos))
  available_agg <- stats::aggregate(
    available_df[, c("Version", fields), drop = FALSE],
    list(Package = available_df$Package),
    function(x) x[1]
  )
  available_agg
}

available_agg_fun <- memoise::memoise(available_agg_fun_raw, cache = cachem::cache_mem(max_age = 15 * 60))

expand_dependency <- function(x) {
  if (length(x) == 1) {
    stopifnot(all(x %in% c("strong", "all", "most")))
    switch(
      x,
      strong = c("Depends", "Imports", "LinkingTo"),
      most = c("Depends", "Imports", "LinkingTo", "Suggests"),
      all = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
      c("Depends", "Imports", "LinkingTo")
    )
  } else {
    stopifnot(all(x %in% c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances")))
    x
  }
}
