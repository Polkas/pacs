replaceNA <- function(vec, with) {
  vec[is.na(vec)] <- with
  vec
}

isNA <- function(x) {
  isTRUE(length(x) == 1 && is.na(x))
}

#' Maximum version across the vector
#' @description Reduce function over the `utils::compareVersion`
#' @inheritParams standard_args
#' @return `character` maximum version
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
#' @inheritParams standard_args
#' @return `character` minimal version
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
#' @inheritParams standard_args
#' @return `numeric` size in bytes, to get MB ten divide by `10**6`.
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

is_online <- function() {
  curl::has_internet()
}

#' Get base R packages
#' @description get base packages, all or only `startup`.
#' @inheritParams standard_args
#' @return `character` vector
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

  paks <- installed_agg[, fields, drop = FALSE]

  if (!is.null(deps)) {
    nams <- installed_agg[, c("Package")]
    paks <- paks[nams %in% deps, , drop = FALSE]
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

installed_agg_fun <- memoise::memoise(installed_agg_fun_raw, cache = cachem::cache_mem(max_age = 30 * 60))

#' List Available Packages at CRAN-like Repositories
#' @description available_packages returns a matrix of details corresponding to packages currently available at one or more repositories. The current list of packages is downloaded over the internet (or copied from a local mirror).
#' @param repos character vector URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' @keywords internal
available_packages <- function(repos) {
  tryCatch(available_packages_raw(repos = repos), error = function(e) NA)
}

available_packages_raw <- memoise::memoise(utils::available.packages, cache = cachem::cache_mem(max_age = 30 * 60))

installed_packages <- function(lib.loc, priority = NULL) {
  installed_packages_raw(lib.loc = lib.loc, priority = priority)
}

installed_packages_raw <- memoise::memoise(utils::installed.packages, cache = cachem::cache_mem(max_age = 30 * 60))

extract_deps <- function(x) {
  splited <- stri_split_fixed(x, ",", )
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

  paks <- available_agg[, fields, drop = FALSE]

  if (!is.null(deps)) {
    nams <- available_agg[, c("Package")]
    paks <- paks[nams %in% deps, , drop = FALSE]
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

available_agg_fun <- memoise::memoise(available_agg_fun_raw, cache = cachem::cache_mem(max_age = 30 * 60))

expand_dependency <- function(fields) {
  if (length(fields) == 1 && fields %in% c("strong", "all", "most")) {
    switch(fields,
      strong = c("Depends", "Imports", "LinkingTo"),
      most = c("Depends", "Imports", "LinkingTo", "Suggests"),
      all = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
    )
  } else {
    stopifnot(all(fields %in% c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances")))
    fields
  }
}

cran_archive_file <- function(pac, version, repos, file) {
  last_version <- pac_last(pac, repos)

  if (isTRUE(!is.null(version) && version != last_version)) {
    base_url <- sprintf("https://cran.r-project.org/src/contrib/Archive/%s", pac)
  } else {
    base_url <- "https://cran.r-project.org/src/contrib"
    version <- last_version
  }

  d_url <- sprintf(
    "%s/%s_%s.tar.gz",
    base_url,
    pac,
    version
  )

  temp_tar <- tempfile(fileext = ".tar.gz")

  download <- try(
    {
      suppressWarnings(utils::download.file(d_url,
        destfile = temp_tar,
        quiet = TRUE
      ))
    },
    silent = TRUE
  )

  if (inherits(download, "try-error")) {
    result <- structure(list(), package = pac, version = version)
  } else {
    temp_dir <- tempdir()
    utils::untar(temp_tar, exdir = temp_dir)
    # tabs are not acceptable
    result <- switch(file,
      DESCRIPTION = as.list(read.dcf(file.path(temp_dir, pac, "DESCRIPTION"))[1, ]),
      NAMESPACE = readLines(file.path(temp_dir, pac, "NAMESPACE"), warn = FALSE)
    )
  }
  unlink(temp_tar)
  result
}

read_html_table <- function(table_lines) {
  rr_range <- grep("</?table[^>]*>", table_lines)
  if (length(rr_range) != 2) {
    return(NA)
  }
  rrr <- table_lines[(rr_range[1] + 1):(rr_range[2] - 1)]
  rrr_all <- paste(rrr, collapse = "\n")
  rrr_html <- read_html(rrr_all)
  list(html = rrr_html, lines = rrr)
}

crandb_json <- function(packages,
                        limit = getOption("pacs.crandb_limit", 100),
                        ntry = getOption("pacs.crandb_ntry", 3),
                        nsleep = getOption("pacs.crandb_nsleep", 0.1)) {
  if (!is_online()) {
    message("No internet connection detected.\n")
    return(NA)
  }

  crandb_url <- sprintf(
    'https://crandb.r-pkg.org/-/allall?keys=["%s"]&limit=%s',
    paste(packages, collapse = '","'),
    limit
  )

  result <- NA
  for (iter in seq_len(ntry)) {
    fetch_call <- try(httr::GET(crandb_url), silent = TRUE)

    if (!inherits(fetch_call, "try-error") && httr::status_code(fetch_call) == 200) {
      result <- jsonlite::fromJSON(httr::content(fetch_call, as = "text", encoding = "UTF-8"))
      break
    }

    Sys.sleep(nsleep)
  }

  result
}
