read_checkpage_raw <- function(pac) {
  rr <- try(readLines(sprintf("https://cran.r-project.org/web/checks/check_results_%s.html", pac), warn = FALSE), silent = TRUE)
  if (!inherits(rr, "try-error")) {
    tableh <- read_html_table(rr)
    rrr <- tableh$lines
    length_rrr <- length(rrr)

    header_raw <- xml_find_all(read_html(rrr[1]), "//th")
    header <- trimws(xml_text(header_raw))
    html_body <- read_html(paste0(rrr[2:length_rrr], collapse = "\n"))
    result_raw <- matrix(trimws(xml_text(xml_find_all(html_body, "/html/body//tr/td"))),
      ncol = length(header),
      byrow = TRUE
    )
    result_raw <- as.data.frame(result_raw)
    colnames(result_raw) <- header

    all_links <- xml_attr(xml_find_all(html_body, "//tr/td/a"), "href")
    all_links <- all_links[grepl("check_flavors.html#", all_links)]
    flavs <- trimws(gsub("check_flavors.html#", "", all_links))
    result_raw$Flavor <- flavs

    result_raw
  } else {
    NA
  }
}

read_checkpage <- memoise::memoise(read_checkpage_raw, cache = cachem::cache_mem(max_age = 30 * 60))

#' Retrieving the R CRAN package check page
#' @description Retrieving the R CRAN package check page.
#' @inheritParams standard_args
#' @return `data.frame`.
#' @note Results are cached for 30 minutes with `memoise` package.
#' If you need to check many packages at once then is recommended usage of `pacs::checked_packages`.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_checkpage("dplyr")
#' }
pac_checkpage <- function(pac) {
  stopifnot((length(pac) == 1) && is.character(pac))
  if (!is_online()) {
    message("No internet connection detected.\n")
    return(NA)
  }

  if (!pac_isin(pac, "https://cran.rstudio.com/")) {
    message(sprintf("%s package is not on CRAN.\n", pac))
    return(NA)
  }

  read_checkpage(pac)
}

#' Checking the R CRAN package check page status
#' @description using package R CRAN check page to validate if there are ANY errors and/or fails and/or warnings and/or notes.
#' @inheritParams standard_args
#' @return `logical` if the package fails under the specified criteria, or
#' `NA` if the required check statuses cannot be retrieved.
#' @note Results are cached for 30 minutes with `memoise` package.
#' If you need to check many packages at once then is recommended usage of `pacs::checked_packages`.
#' The used repository `https://cran.rstudio.com/`.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_checkred("dplyr")
#' pacs::pac_checkred("dplyr", scope = c("ERROR"))
#' pacs::pac_checkred("dplyr",
#'   scope = c("ERROR", "FAIL", "WARN"),
#'   flavors = pacs::match_flavors()
#' )
#' }
pac_checkred <- function(pac, scope = c("ERROR", "FAIL"), flavors = NULL) {
  if (!is_online()) {
    message("No internet connection detected.\n")
    return(NA)
  }

  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.null(scope) || (is.character(scope) && all(scope %in% c("ERROR", "FAIL", "WARN", "NOTE"))))
  stopifnot(is.null(flavors) || is.character(flavors))
  if (length(flavors)) {
    available_flavors <- cran_flavors()
    if (!is.data.frame(available_flavors) || !"Flavor" %in% names(available_flavors)) {
      message("Failed to retrieve CRAN check flavors.\n")
      return(NA)
    }
    stopifnot(all(flavors %in% available_flavors$Flavor))
  }

  if (!pac_isin(pac, "https://cran.rstudio.com/")) {
    message(sprintf("%s package is not on CRAN.\n", pac))
    return(NA)
  }
  if (!length(scope)) return(FALSE)

  result_raw <- pac_checkpage(pac)
  if (!is.data.frame(result_raw) ||
      !all(c("Flavor", "Status") %in% names(result_raw)) ||
      nrow(result_raw) == 0) {
    message("Failed to retrieve package check statuses.\n")
    return(NA)
  }
  if (length(flavors)) {
    result_raw <- result_raw[result_raw$Flavor %in% flavors, , drop = FALSE]
    if (nrow(result_raw) == 0) {
      message("No package check statuses found for the selected flavors.\n")
      return(NA)
    }
  }
  statuses <- sub("\\*$", "", trimws(result_raw$Status))
  if (any(statuses %in% scope)) return(TRUE)
  if (anyNA(statuses) || any(!statuses %in% c("OK", "ERROR", "FAIL", "WARN", "NOTE"))) {
    message("Some package check statuses are unavailable.\n")
    return(NA)
  }
  FALSE
}

#' Retrieving all R CRAN packages check pages statuses.
#' @description Retrieving all R CRAN packages check pages statuses.
#' The data is downloaded from `https://cran.r-project.org/web/checks/check_summary_by_package.html`.
#' @return `data.frame` with the same structure as the html table on `https://cran.r-project.org/web/checks/check_summary_by_package.html`.
#' @note Results are cached for 30 minutes with `memoise` package.
#' Some packages could be duplicated as not all tests are performed for a new version so two versions still coexists.
#' Checks with asterisks (*) indicate that checking was not fully performed, this is a case for less than 1% of all packages.
#' @export
#' @examples
#' \dontrun{
#' pacs::checked_packages()
#' }
checked_packages <- function() {
  if (!is_online()) {
    message("No internet connection detected.\n")
    return(NA)
  }
  packages <- read_checkred_packages(url = "https://cran.r-project.org/web/checks/check_summary_by_package.html")
  if (is.data.frame(packages)) {
    result <- packages
  } else {
    message("Failed to fetch check summary by package.\n")
    result <- NA
  }
  result
}

read_checkred_packages_raw <- function(url = "https://cran.r-project.org/web/checks/check_summary_by_package.html") {
  rr <- try(readLines(url, warn = FALSE), silent = TRUE)
  if (!inherits(rr, "try-error")) {
    tableh <- read_html_table(rr)
    rrr <- tableh$lines
    length_rrr <- length(rrr)

    header_raw <- xml_find_all(read_html(rrr[1]), "//th")
    header <- trimws(xml_text(header_raw))
    header_machines <- trimws(gsub("check_flavors.html#", "", xml_attr(xml_find_all(header_raw, "//a"), "href")))
    which_machines <- grep("r-", header)
    header[which_machines] <- header_machines

    nmachines <- length(header_machines)
    nmachines_empty <- rep("", nmachines)
    checks_text <- lapply(rrr[2:length_rrr], function(e) {
      res_true <- trimws(xml_text(xml_find_all(read_html(paste0(e, collapse = "\n")), "/html/body//tr/td[@class]")))
      if (length(res_true) == nmachines) {
        res_true
      } else {
        nmachines_empty
      }
    })
    checks_text_bind <- do.call(rbind, checks_text)

    html_base <- suppressWarnings(read_html(paste0(rrr[2:length_rrr], collapse = "\n")))
    xml_base <- xml_find_all(html_base, "/html/body//tr")
    package_names <- trimws(xml_text(xml_find_all(xml_base, "td[1]")))
    package_versions <- trimws(xml_text(xml_find_all(xml_base, "td[2]")))
    package_maintainer <- trimws(xml_text(xml_find_all(xml_base, "td[last()-1]")))
    package_priority <- trimws(xml_text(xml_find_all(xml_base, "td[last()]")))

    result_raw <- as.data.frame(cbind(package_names, package_versions, checks_text_bind, package_maintainer, package_priority))
    colnames(result_raw) <- header
  } else {
    result_raw <- NA
  }
  result_raw
}

read_checkred_packages <- memoise::memoise(read_checkred_packages_raw, cache = cachem::cache_mem(max_age = 30 * 60))

read_cran_flavours_raw <- function(url = "https://cran.r-project.org/web/checks/check_flavors.html") {
  rr <- try(readLines(url, warn = FALSE), silent = TRUE)
  if (!inherits(rr, "try-error")) {
    tableh <- read_html_table(rr)
    rrr_html <- tableh$html

    header <- trimws(xml_text(xml_find_all(rrr_html, "//th")))
    result_raw <- as.data.frame(matrix(trimws(xml_text(xml_find_all(rrr_html, "//td"))),
      ncol = length(header),
      byrow = TRUE
    ))
    colnames(result_raw) <- header
    result_raw$Flavor <- gsub("\\+", "_", result_raw$Flavor)
  } else {
    result_raw <- NA
  }

  result_raw
}

read_cran_flavours <- memoise::memoise(read_cran_flavours_raw, cache = cachem::cache_mem(max_age = 30 * 60))

#' Retrieving all R CRAN servers flavors
#' @description Retrieving all R CRAN servers flavors.
#' The data is downloaded from `https://cran.r-project.org/web/checks/check_flavors.html`.
#' @return `data.frame` with the same structure as the html table on `https://cran.r-project.org/web/checks/check_flavors.html`.
#' @note Results are cached for 30 minutes with `memoise` package.
#' @export
#' @examples
#' \dontrun{
#' pacs::cran_flavors()
#' }
cran_flavors <- function() {
  read_cran_flavours(url = "https://cran.r-project.org/web/checks/check_flavors.html")
}

read_bio_releases_raw <- function(url = "https://bioconductor.org/config.yaml") {
  config <- yaml::yaml.load(
    paste(suppressWarnings(readLines(url, warn = FALSE)), collapse = "\n"),
    eval.expr = FALSE
  )
  if (!is.list(config) || !is.character(config$release_version) ||
      length(config$release_version) != 1L || is.na(config$release_version) ||
      !is.list(config$release_dates) ||
      !is.list(config$r_ver_for_bioc_ver)) {
    stop("current release, release dates, or R version mappings are missing")
  }

  release_dates <- unlist(config$release_dates, use.names = TRUE)
  r_versions <- unlist(config$r_ver_for_bioc_ver, use.names = TRUE)
  releases <- names(release_dates)
  if (!length(releases) || anyDuplicated(releases) ||
      !length(r_versions) || is.null(names(r_versions)) ||
      anyDuplicated(names(r_versions))) {
    stop("release dates or R version mappings are invalid")
  }

  if (!config$release_version %in% releases) {
    stop("the current release date is missing")
  }
  released <- package_version(releases) <= package_version(config$release_version)
  releases <- releases[released]
  release_dates <- release_dates[released]
  dates <- as.Date(as.character(release_dates), format = "%m/%d/%Y")
  if (anyNA(dates)) {
    stop("one or more release dates are invalid")
  }
  ordering <- order(package_version(releases), decreasing = TRUE)
  data.frame(
    Release = releases[ordering],
    Date = dates[ordering],
    `Software packages` = rep(NA_character_, length(releases)),
    R = unname(as.character(r_versions[releases[ordering]])),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

read_bio_releases <- memoise::memoise(read_bio_releases_raw, cache = cache_mem(max_age = 30 * 60))

#' Retrieving all Bioconductor releases
#' @description Retrieving all Bioconductor releases.
#' The data is downloaded from Bioconductor's machine-readable
#' `https://bioconductor.org/config.yaml`.
#' @return `data.frame` with `Release`, `Date`, `Software packages`, and `R`
#' columns, or `NA` if the configuration cannot be read. The configuration
#' does not provide historical software-package counts, so `Software packages`
#' is `NA`. The `R` column is `NA` for releases without an R version mapping.
#' @note Only versions up to the current Bioconductor release with a release
#' date are included, newest first.
#' Dates and R versions follow the Bioconductor configuration and may differ
#' from other historical sources. Successful results are cached for 30 minutes
#' with the `memoise` package; failures are not cached.
#' @export
#' @examples
#' \dontrun{
#' pacs::bio_releases()
#' }
bio_releases <- function() {
  tryCatch(read_bio_releases(), error = function(e) {
    message("Failed to read Bioconductor release configuration: ", conditionMessage(e), "\n")
    NA
  })
}

#' CRAN and Bioconductor repositories
#' @description CRAN and Bioconductor repositories.
#' The newest Bioconductor release for the specific R version is assumed.
#' @param version `character` the Bioconductor release.
#' By default the newest Bioconductor release for the specific R version is assumed, if not available only CRAN repository is returned.
#' Available Bioconductor versions for your R version could be checked with `pacs::bio_releases()`. Default NULL
#' @return `named character` vector of repositories.
#' @note The Internet connection is needed to get Bioconductor repositories.
#' If the release configuration is unavailable or no release matches, a
#' message is issued and only the CRAN repository is returned.
#' @export
#' @examples
#' \dontrun{
#' pacs::biocran_repos()
#' }
biocran_repos <- function(version = NULL) {
  cran <- c(CRAN = "https://cran.rstudio.com/")
  if (!is_online()) {
    message("No internet connection detected; using the CRAN repository only.\n")
    return(cran)
  }

  releases <- bio_releases()
  if (!is.data.frame(releases)) {
    message("Bioconductor release information is unavailable; using the CRAN repository only.\n")
    return(cran)
  }

  Rv <- paste0(R.Version()$major, ".", stri_split_fixed(R.Version()$minor, ".")[[1]][1])
  bio_ok <- releases$Release[!is.na(releases$R) & releases$R == Rv]
  if (!length(bio_ok) || (!is.null(version) && !isTRUE(version %in% bio_ok))) {
    message("No matching Bioconductor release for R ", Rv, "; using the CRAN repository only.\n")
    return(cran)
  }
  if (is.null(version)) version <- bio_ok[1]

  c(
    BioCsoft = sprintf("https://bioconductor.org/packages/%s/bioc", version),
    BioCann = sprintf("https://bioconductor.org/packages/%s/data/annotation", version),
    BioCexp = sprintf("https://bioconductor.org/packages/%s/data/experiment", version),
    BioCworkflows = sprintf("https://bioconductor.org/packages/%s/workflows", version),
    BioCbooks = if (utils::compareVersion(version, "3.12") >= 0) sprintf("https://bioconductor.org/packages/%s/books", version) else NULL,
    cran
  )
}

#' Get all matched CRAN servers to the local `OS`
#' @description CRAN servers matched to the local `OS`.
#' @return `character` vector matched server names.
#' @note The Internet connection is needed to use the function.
#' @export
#' @examples
#' \dontrun{
#' pacs::match_flavors()
#' }
match_flavors <- function() {
  if (!is_online()) {
    message("No internet connection detected.\n")
    return(NA)
  }
  flavors_df <- cran_flavors()
  if (isNA(flavors_df)) {
    return(NA)
  }
  flavors <- flavors_df$Flavor
  switch(Sys.info()[["sysname"]],
    Windows = {
      flavors[grepl("windows", flavors)]
    },
    Linux = {
      flavors[grepl("linux", flavors)]
    },
    Darwin = {
      flavors[grepl("macos", flavors)]
    }
  )
}
