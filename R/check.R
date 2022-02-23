read_checkpage_raw <- function(pac) {
  rr <- try(readLines(sprintf("https://cran.r-project.org/web/checks/check_results_%s.html", pac), warn = FALSE), silent = TRUE)
  if (!inherits(rr, "try-error")) {
    rrr_html <- read_html_table(rr)
    header <- trimws(xml_text(xml_find_all(rrr_html, "//th")))
    result_raw <- as.data.frame(matrix(trimws(xml_text(xml_find_all(rrr_html, "//td"))),
      ncol = length(header),
      byrow = TRUE
    ))
    colnames(result_raw) <- header
    result_raw$Flavor <- gsub("\\+", "_", result_raw$Flavor)
    result_raw
  } else {
    NA
  }
}

read_checkpage <- memoise::memoise(read_checkpage_raw, cache = cachem::cache_mem(max_age = 30 * 60))

#' Retrieving the R CRAN package check page
#' @description Retrieving the R CRAN package check page.
#' @param pac character a package name.
#' @return data.frame.
#' @note Results are cached for 30 minutes with `memoise` package.
#' If you need to check many packages at once then is recommended usage of `pacs::checked_packages`.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' \dontrun{
#' pac_checkpage("dplyr")
#' }
pac_checkpage <- function(pac) {
  stopifnot((length(pac) == 1) && is.character(pac))
  if (!is_online()) {
    return(NA)
  }

  if (!pac_isin(pac, "https://cran.rstudio.com/")) {
    return(NA)
  }

  read_checkpage(pac)
}

#' Checking the R CRAN package check page status
#' @description using package R CRAN check page to validate if there are ANY errors and/or fails and/or warnings and/or notes.
#' @param pac character a package name.
#' @param scope character vector scope of the check, accepted values c("ERROR", "FAIL", "WARN", "NOTE"). Default c("ERROR", "FAIL")
#' @param flavors character vector of CRAN machines to consider, which might be retrieved with `pacs::cran_flavors()$Flavor`. By default all CRAN machines are considered, NULL value. Default NULL
#' @return logical if the package fail under specified criteria.
#' @note Results are cached for 30 minutes with `memoise` package.
#' If you need to check many packages at once then is recommended usage of `pacs::checked_packages`.
#' The used repository `https://cran.rstudio.com/`.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' \dontrun{
#' pac_checkred("dplyr")
#' pac_checkred("dplyr", scope = c("ERROR"))
#' pac_checkred("dplyr",
#'   scope = c("ERROR", "FAIL", "WARN"),
#'   flavors = c(
#'     "r-devel-linux-x86_64-debian-clang",
#'     "r-devel-linux-x86_64-debian-gcc"
#'   )
#' )
#' }
pac_checkred <- function(pac, scope = c("ERROR", "FAIL"), flavors = NULL) {
  stopifnot(all(scope %in% c("ERROR", "FAIL", "WARN", "NOTE")))
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(length(scope) == 0 || all(scope %in% c("ERROR", "FAIL", "WARN", "NOTE")) &&
    is.null(flavors) || all(flavors %in% cran_flavors()$Flavor))
  if (!is_online()) {
    return(NA)
  }

  if (!pac_isin(pac, "https://cran.rstudio.com/")) {
    return(NA)
  }

  result_raw <- pac_checkpage(pac)
  if (length(flavors)) {
    any(result_raw[result_raw$Flavor %in% flavors, ]$Status %in% scope)
  } else {
    any(result_raw$Status %in% scope)
  }
}

#' Retrieving all R CRAN packages check pages statuses.
#' @description Retrieving all R CRAN packages check pages statuses.
#' The data is downloaded from `https://cran.r-project.org/web/checks/check_summary_by_package.html`.
#' @return data.frame with the same structure as the html table on `https://cran.r-project.org/web/checks/check_summary_by_package.html`.
#' @note Results are cached for 30 minutes with `memoise` package.
#' Some packages could be duplicated as not all tests are performed for a new version so two versions still coexists.
#' Checks with asterisks (*) indicate that checking was not fully performed, this is a case for less than 1% of all packages.
#' @export
#' @examples
#' \dontrun{
#' checked_packages()
#' }
checked_packages <- function() {
  if (!is_online()) {
    return(NA)
  }
  packages <- read_checkred_packages()
  if (is.data.frame(packages)) {
    result <- packages
  } else {
    result <- NA
  }
  result
}

read_checkred_packages_raw <- function() {
  rr <- try(readLines("https://cran.r-project.org/web/checks/check_summary_by_package.html", warn = FALSE), silent = TRUE)
  if (!inherits(rr, "try-error")) {
    length_rr <- length(rr)
    rr_range <- grep("</?table[^>]*>", rr)
    if (length(rr_range) != 2) {
      return(NA)
    }
    rrr <- rr[(rr_range[1] + 1):(rr_range[2] - 1)]
    header_raw <- xml_find_all(read_html(rrr[1]), "//th")
    header <- trimws(xml_text(header_raw))
    header_machines <- trimws(gsub("check_flavors.html#", "", xml_attr(xml_find_all(header_raw, "//a"), "href")))
    which_machines <- grep("r-", header)
    header[which_machines] <- header_machines
    length_rrr <- length(rrr)
    result_raw <- matrix(trimws(xml_text(xml_find_all(read_html(paste0(rrr[2:length_rrr], collapse = "\n")), "/html/body//tr/td"))),
      ncol = length(header),
      byrow = TRUE
    )
    result_raw <- as.data.frame(result_raw)
    colnames(result_raw) <- header
  } else {
    result_raw <- NA
  }
  result_raw
}

read_checkred_packages <- memoise::memoise(read_checkred_packages_raw, cache = cachem::cache_mem(max_age = 30 * 60))

read_cran_flavours_raw <- function() {
  rr <- try(readLines("https://cran.r-project.org/web/checks/check_flavors.html", warn = FALSE), silent = TRUE)
  if (!inherits(rr, "try-error")) {
    rrr_html <- read_html_table(rr)
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
#' @return data.frame with the same structure as the html table on `https://cran.r-project.org/web/checks/check_flavors.html`.
#' @note Results are cached for 30 minutes with `memoise` package.
#' @export
#' @examples
#' \dontrun{
#' cran_flavors()
#' }
cran_flavors <- function() {
  read_cran_flavours()
}

read_bio_releases_raw <- function() {
  rr <- try(readLines("https://www.bioconductor.org/about/release-announcements/", warn = FALSE), silent = TRUE)
  if (!inherits(rr, "try-error")) {
    rrr_html <- read_html_table(rr)
    header <- trimws(xml_text(xml_find_all(rrr_html, "//th")))
    result_raw <- as.data.frame(matrix(trimws(xml_text(xml_find_all(rrr_html, "//td"))),
      ncol = length(header), byrow = TRUE
    ))
    colnames(result_raw) <- header
    result_raw$Date <- as.Date(result_raw$Date, format = "%B %d, %Y")
    result_raw$Release <- as.character(result_raw$Release)
    result_raw$R <- as.character(result_raw$R)
  } else {
    result_raw <- NA
  }

  result_raw
}

read_bio_releases <- memoise::memoise(read_bio_releases_raw, cache = cachem::cache_mem(max_age = 30 * 60))

#' Retrieving all Bioconductor releases
#' @description Retrieving all Bioconductor releases.
#' The data is downloaded from `https://www.bioconductor.org/about/release-announcements/`.
#' @return data.frame with the same structure as the html table on `https://www.bioconductor.org/about/release-announcements/`.
#' @note Results are cached for 30 minutes with `memoise` package.
#' @export
#' @examples
#' \dontrun{
#' bio_releases()
#' }
bio_releases <- function() {
  read_bio_releases()
}

#' CRAN and Bioconductor repositories
#' @description CRAN and Bioconductor repositories.
#' The newest Bioconductor release for the specific R version is assumed.
#' @param version character the Bioconductor release.
#' By default the newest Bioconductor release for the specific R version is assumed, if not available only CRAN repository is returned.
#' Available Bioconductor versions for your R version could be checked with `pacs::bio_releases()`. Default NULL
#' @return named character vector of repositories.
#' @note The Internet connection is needed to get Bioconductor repositories.
#' @export
#' @examples
#' \dontrun{
#' biocran_repos()
#' }
biocran_repos <- function(version = NULL) {
  if (is_online()) {
    Rv <- paste0(R.Version()$major, ".", stri_split_fixed(R.Version()$minor, ".")[[1]][1])
    bio_ok <- bio_releases()$Release[match(Rv, bio_releases()$R)]
    if (!(is.null(version) || (!is.null(version) && isTRUE(version %in% bio_ok)))) {
      return(c(CRAN = "https://cran.rstudio.com/"))
    }
    if (is.null(version)) version <- utils::head(bio_ok, 1)
    if (isNA(version)) {
      c(CRAN = "https://cran.rstudio.com/")
    } else {
      c(
        BioCsoft = sprintf("https://bioconductor.org/packages/%s/bioc", version),
        BioCann = sprintf("https://bioconductor.org/packages/%s/data/annotation", version),
        BioCexp = sprintf("https://bioconductor.org/packages/%s/data/experiment", version),
        BioCworkflows = sprintf("https://bioconductor.org/packages/%s/workflows", version),
        BioCbooks = if (utils::compareVersion(version, "3.12") >= 0) sprintf("https://bioconductor.org/packages/%s/books", version) else NULL,
        CRAN = "https://cran.rstudio.com/"
      )
    }
  } else {
    c(CRAN = "https://cran.rstudio.com/")
  }
}
