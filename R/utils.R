
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
      suppressWarnings(readLines(site, n = 1))
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
#' pacs_base()
#' pacs_base(startup = TRUE)
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
  rownames(utils::installed.packages(priority = "base"))
}

pacs_base_all <- memoise::memoise(pacs_base_all_raw)

installed_descriptions <- function(lib.loc, fields, deps = NULL) {
  installed_agg <- installed_agg_fun(lib.loc, fields)

  paks <- installed_agg[, fields]
  nams <- installed_agg[, c("Package")]
  rownames(paks) <- nams

  if (!is.null(deps)) {
    paks <- paks[nams %in% deps, ]
  }

  joint_cols <- apply(paks, 1, function(x) paste(x, collapse = ","))

  desc_e <- extract_deps(joint_cols)

  packages <- desc_e$packages
  versions <- desc_e$versions

  joint <- do.call(rbind, lapply(
    seq_along(packages),
    function(x) {
      data.frame(
        Version = replaceNA(versions[[x]], ""),
        Package = replace(packages[[x]], versions[[x]] == "NA", NA),
        stringsAsFactors = FALSE
      )
    }
  ))
  res_agg <- stats::aggregate(
    joint[, c("Version"), drop = FALSE],
    list(Package = joint$Package),
    compareVersionsMax
  )

  res_agg$Version[is.na(res_agg$Version)] <- ""

  res_agg
}

installed_agg_fun_raw <- function(lib.loc = NULL, fields) {
  installed_df <- as.data.frame(utils::installed.packages(lib.loc = NULL))
  installed_agg <- stats::aggregate(
    installed_df[, c("Version", fields), drop = FALSE],
    list(Package = installed_df$Package),
    function(x) x[1]
  )
  installed_agg
}

installed_agg_fun <- memoise::memoise(installed_agg_fun_raw)

available_packages <- function(repos = "https://cran.rstudio.com/") {
  available_packages_raw(repos = repos)
}

available_packages_raw <- memoise::memoise(utils::available.packages)

extract_deps <- function(x) {
  splited <- strsplit(x, ",")
  trimed <- lapply(splited, trimws)
  v_reg <- function(x) sapply(regmatches(x, regexec("([0-9\\.-]+)\\)", x, perl = TRUE)), function(i) `[`(i, 2))
  versions <- lapply(trimed, v_reg)
  v_pac <- function(x) sapply(strsplit(x, "[ \n\\(]"), function(x) `[`(x, 1))
  pacs <- lapply(trimed, v_pac)
  list(packages = pacs, versions = versions)
}

last_version_raw <- function(pac) {
  available_packages()[rownames(available_packages()) == pac, "Version"]
}

last_version_fun <- memoise::memoise(last_version_raw)

is_last_release <- function(pac, version = NULL, at = NULL) {
  stopifnot(xor(!is.null(version), !is.null(at)) || (is.null(version) && is.null(at)))

  if (!pac %in% rownames(available_packages())) {
    return(NA)
  }

  last_version <- available_packages()[rownames(available_packages()) == pac, "Version"]

  is_installed <- isTRUE(pac %in% rownames(utils::installed.packages()))

  if (is.null(version) && is.null(at)) {
    if (is_installed) {
      version <- pac_description(pac, local = TRUE)$Version
    } else {
      return(FALSE)
    }
  } else if (is.null(version) && !is.null(at)) {
    version <- utils::tail(pac_timemachine(pac = pac, at = at), 1)$Version
  }

  isTRUE(utils::compareVersion(last_version, version) == 0)
}


available_descriptions <- function(repos, fields, deps = NULL) {
  available_agg <- available_agg_fun(repos, fields)

  paks <- available_agg[, fields]
  nams <- available_agg[, c("Package")]
  rownames(paks) <- nams

  if (!is.null(deps)) {
    paks <- paks[nams %in% deps, ]
  }

  joint_cols <- apply(paks, 1, function(x) paste(x, collapse = ","))

  desc_e <- extract_deps(joint_cols)

  packages <- desc_e$packages
  versions <- desc_e$versions

  joint <- do.call(rbind, lapply(
    seq_along(packages),
    function(x) {
      data.frame(
        Version = replaceNA(versions[[x]], ""),
        Package = replace(packages[[x]], versions[[x]] == "NA", NA),
        stringsAsFactors = FALSE
      )
    }
  ))
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

available_agg_fun <- memoise::memoise(available_agg_fun_raw)

is_red_check_raw <- function(pac, scope = c("ERROR", "WARN")) {
  if (!pac %in% rownames(available_packages())) {
    return(NA)
  }
  any(grepl(sprintf("Result: (?:%s)", paste(scope, collapse = "|")),
            get_cran_check_page(pac),
    perl = TRUE
  ))
}

get_cran_check_page_raw <- function(pac) {
  readLines(sprintf("https://cran.r-project.org/web/checks/check_results_%s.html", pac))
}

get_cran_check_page <- memoise::memoise(get_cran_check_page_raw)

#' Checking the R CRAN package check page status
#' @description using R CRAN package check page to validate if there are ANY error and/or warning and/or note.
#' @param pac character a package name.
#' @param scope character vector scope of the check, accepted values c("ERROR", "WARN", "NOTE"). Default c("ERROR", "WARN")
#' @return logical if the package fail under specified criteria.
#' @export
#' @examples
#' pac_checkred("dplyr")
#' pac_checkred("dplyr", scope = c("ERROR"))
pac_checkred <- function(pac, scope = c("ERROR", "WARN")) {
  stopifnot(all(scope %in% c("ERROR", "WARN", "NOTE")))

  is_red_check_raw(pac, scope)
}

#' Checking the R CRAN packages check pages statuses
#' @description using R CRAN packages check pages to validate if there are ANY error and/or warning and/or note.
#' @param pacs character vector packages names.
#' @param scope character vector scope of the check, accepted values c("ERROR", "WARN", "NOTE"). Default c("ERROR", "WARN")
#' @return logical named vector if packages fail under specified criteria.
#' @export
#' @examples
#' pacs_checkred(c("dplyr", "devtools"))
#' pacs_checkred("dplyr", scope = c("ERROR"))
pacs_checkred <- function(pacs, scope = c("ERROR", "WARN", "NOTE")) {
  stopifnot(all(scope %in% c("ERROR", "WARN", "NOTE")))
  stopifnot(is.null(pacs) || is.character(pacs))

  checks <- vapply(
    pacs,
    function(p) is_red_check_raw(p, scope),
    logical(1)
  )

  stats::setNames(checks, pacs)
}
