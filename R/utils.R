
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

#' Minimum version across vector
#' @description Reduce function over the utils::compareVersion
#' @param vec character vector
#' @param na.rm logical if to remove NA values.
#' @return character
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

is_online <- function(site = "http://example.com/") {
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
#' @param startup logical include only stratup packages. Default: FALSE
#' @return character vector
#' @export
#'
pacs_base <- function(startup = FALSE) {
  stopifnot(is.logical(startup))
  if (startup) {
    c(getOption("defaultPackages"), "base")
  } else {
    rownames(utils::installed.packages(priority = "base"))
  }
}

installed_descriptions <- function(lib.loc, fields) {
  installed_agg <- installed_agg_fun(lib.loc, fields)

  paks <- installed_agg[, fields]
  nams <- rownames(paks)
  rownames(paks) <- nams

  joint_cols <- apply(paks, 1, function(x) paste(x, sep = ","))

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

installed_agg_fun <- function(lib.loc = NULL, fields) {
  installed_df <- as.data.frame(utils::installed.packages(lib.loc = NULL))
  installed_agg <- stats::aggregate(
    installed_df[, c("Version", fields), drop = FALSE],
    list(Package = installed_df$Package),
    function(x) x[1]
  )
  installed_agg
}


available_packages <- utils::available.packages(repos = "http://cran.rstudio.com/")

extract_deps <- function(x) {
  splited <- strsplit(x, ",")
  trimed <- lapply(splited, trimws)
  v_reg <- function(x) sapply(regmatches(x, regexec("([0-9\\.-]+)\\)", x, perl = TRUE)), function(i) `[`(i, 2))
  versions <- lapply(trimed, v_reg)
  v_pac <- function(x) sapply(strsplit(x, "[ \n\\(]"), function(x) `[`(x, 1))
  pacs <- lapply(trimed, v_pac)
  list(packages = pacs, versions = versions)
}

is_last_release <- function(pac, version = NULL, at = NULL) {
  stopifnot(xor(!is.null(version), !is.null(at)))

  if (!pac %in% rownames(available_packages)) {
    return(NA)
  }

  last_version <- available_packages[rownames(available_packages) == pac, "Version"]

  if (is.null(version)) {
    version <- utils::tail(pac_timemachine(pac = pac, at = at), 1)$Version
  }

  isTRUE(utils::compareVersion(last_version, version) == 0)
}
