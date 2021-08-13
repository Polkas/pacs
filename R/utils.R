
replaceNA <- function(vec, with) {
  vec[is.na(vec)] <- with
  vec
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

dir_size <- function(path, recursive = TRUE) {
  stopifnot(is.character(path))
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
  rownames(installed_packages(priority = "base"))
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
        Package = unlist(lapply(seq_along(packages), function(x)  replace(packages[[x]], packages[[x]] == "NA", NA))),
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

installed_agg_fun_raw <- function(lib.loc = NULL, fields) {
  installed_df <- as.data.frame(installed_packages(lib.loc = NULL))
  installed_agg <- stats::aggregate(
    installed_df[, c("Version", fields), drop = FALSE],
    list(Package = installed_df$Package),
    function(x) x[1]
  )
  installed_agg
}

installed_agg_fun <- memoise::memoise(installed_agg_fun_raw, cache = cachem::cache_mem(max_age = 60*60))

#' List Available Packages at CRAN-like Repositories
#' @description available_packages returns a matrix of details corresponding to packages currently available at one or more repositories. The current list of packages is downloaded over the internet (or copied from a local mirror).
#' @param repos character vector, the base URL(s) of the repositories to use. Default `pacs::biocran_repos()`
available_packages <- function(repos = biocran_repos()) {
  available_packages_raw(repos = repos)
}

available_packages_raw <- memoise::memoise(utils::available.packages, cache = cachem::cache_mem(max_age = 60*60))

installed_packages <- function(lib.loc = NULL, priority = NULL) {
  installed_packages_raw(lib.loc = lib.loc, priority = priority)
}

installed_packages_raw <- memoise::memoise(utils::installed.packages, cache = cachem::cache_mem(max_age = 60*60))

extract_deps <- function(x) {
  splited <- stri_split_fixed(x, ",")
  trimed <- lapply(splited, stri_trim)
  v_reg <- function(x) vapply(stringi::stri_match_all(x, regex = "([0-9\\.-]+)\\)"),
                              function(i) `[`(i, 2),
                              character(1))
  versions <- lapply(trimed, v_reg)
  v_pac <- function(x) vapply(stri_split_regex(x, "[ \n\\(]"),
                              function(x) `[[`(x, 1),
                              character(1))
  pacs <- lapply(trimed, v_pac)
  list(packages = pacs, versions = versions)
}

last_version_raw <- function(pac , repos) {
  which_p <- rownames(available_packages(repos = repos)) == pac
  if (sum(which_p, na.rm = TRUE) > 0) {
    available_packages(repos = repos)[which_p, "Version"]
  } else {
    NA
  }
}

last_version_fun <- memoise::memoise(last_version_raw, cache = cachem::cache_mem(max_age = 60*60))

#' Getting the most recent package version
#' @description using `utils::available.packages` to get the newest package version.
#' @param pac character a package name.
#' @param repos character the base URL of the repository to use. Default `pacs::biocran_repos()`
#' @return character most recent package version.
#' @note Results are cached for 1 hour with `memoise` package.
#' @export
#' @examples
#' pac_last("dplyr")
pac_last <- function(pac, repos = biocran_repos()) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.character(repos))

  if (isTRUE(!pac %in% rownames(available_packages(repos = repos)))) {
    return(NA)
  }

  last_version_fun(pac, repos = repos)
}


is_cran <- function(pac) {
  stopifnot((length(pac) == 1) && is.character(pac))
  if (isTRUE(!pac %in% rownames(available_packages(repos = "https://cloud.r-project.org")))) {
    FALSE
  } else {
    TRUE
  }
}

#' Simple wrapper around `BiocManager::repositories`
#' @description Simple wrapper around `BiocManager::repositories`, suppress messages which are expected e.g. for RStudio users.
#' @param ... optional `BiocManager::repositories` arguments.
#' @return named character vector of repositories.
#' @export
#' @examples
#' biocran_repos()
#' biocran_repos(version = "3.13")
biocran_repos <- function(...) {
  suppressMessages(BiocManager::repositories(...))
}

is_last_release <- function(pac, version = NULL, at = NULL, lib.loc = NULL, repos = c("https://bioconductor.org/packages/3.13/bioc",
                                                                                      "https://bioconductor.org/packages/3.13/data/annotation",
                                                                                      "https://bioconductor.org/packages/3.13/data/experiment",
                                                                                      "https://bioconductor.org/packages/3.13/workflows",
                                                                                      "https://bioconductor.org/packages/3.13/books",
                                                                                      "https://cloud.r-project.org")) {
  stopifnot(xor(!is.null(version), !is.null(at)) || (is.null(version) && is.null(at)))

  if (!pac %in% rownames(available_packages(repos = repos))) {
    return(NA)
  }

  last_version <- pac_last(pac, repos = repos)

  is_installed <- isTRUE(pac %in% rownames(installed_packages(lib.loc = lib.loc)))

  if (isTRUE(is.null(version) && is.null(at))) {
    if (is_installed) {
      version <- pac_description(pac, local = TRUE)$Version
    } else {
      return(FALSE)
    }
  } else if (isTRUE(is.null(version) && !is.null(at))) {
    version <- utils::tail(pac_timemachine(pac = pac, at = at), 1)$Version
  }

  isTRUE(utils::compareVersion(last_version, version) == 0)
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
    Package = unlist(sapply(seq_along(packages), function(x)  replace(packages[[x]], packages[[x]] == "NA", NA))),
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

available_agg_fun_raw <- function(repos = "https://cran.rstudio.com/", fields) {
  available_df <- as.data.frame(available_packages(repos = repos))
  available_agg <- stats::aggregate(
    available_df[, c("Version", fields), drop = FALSE],
    list(Package = available_df$Package),
    function(x) x[1]
  )
  available_agg
}

available_agg_fun <- memoise::memoise(available_agg_fun_raw, cache = cachem::cache_mem(max_age = 60*60))
