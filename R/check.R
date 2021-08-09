read_checkpage_raw <- function(pac) {
  rr <- try(readLines(sprintf("https://cran.r-project.org/web/checks/check_results_%s.html", pac)), silent = TRUE)
  if (!inherits(rr, "try-error")) {
    rr_range <- grep("</?table[^>]*>", rr)
    rrr <- rr[(rr_range[1] + 1):(rr_range[2] - 1)]
    header <- trimws(xml_text(xml_find_all(read_html(rrr[1]), "//th")))

    result_raw <- as.data.frame(matrix(trimws(xml_text(xml_find_all(read_html(paste(rrr[2:length(rrr)], collapse = "\n")), "//td"))),
                                       ncol = length(header),
                                       nrow = length(rrr) - 1, byrow = TRUE))
    colnames(result_raw) <- header
    result_raw$Flavor <- gsub("\\+", "_", result_raw$Flavor)
    result_raw
  } else {
    NA
  }
}

read_checkpage <- memoise::memoise(read_checkpage_raw, cache = cachem::cache_mem(max_age = 60*60))

#' Retrieving the package R CRAN check page
#' @description Retrieving the R CRAN package check page.
#' @param pac character a package name.
#' @param repos character the base URL of the repository to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return data.frame.
#' @note Results are cached for 1 hour with `memoise` package.
#' If you need to check many packages at once then is recommended usage of `pacs::checked_packages`.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' pac_checkpage("dplyr")
pac_checkpage <- function(pac, repos = "https://cran.rstudio.com/") {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(pac %in% rownames(available_packages(repos = repos)))
  read_checkpage(pac)
}

#' Checking the package R CRAN check page status
#' @description using package R CRAN check page to validate if there are ANY errors and/or fails and/or warnings and/or notes.
#' @param pac character a package name.
#' @param scope character vector scope of the check, accepted values c("ERROR", "FAIL", "WARN", "NOTE"). Default c("ERROR", "FAIL")
#' @param flavors character vector of CRAN machines to consider, which might be retrieved with `pacs::cran_flavors()$Flavor`. By default all CRAN machines are considered, NULL value. Default NULL
#' @param repos character the base URL of the repository to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return logical if the package fail under specified criteria.
#' @note Results are cached for 1 hour with `memoise` package.
#' If you need to check many packages at once then is recommended usage of `pacs::checked_packages`.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' pac_checkred("dplyr")
#' pac_checkred("dplyr", scope = c("ERROR"))
#' pac_checkred("dplyr",
#'              scope = c("ERROR", "FAIL", "WARN"),
#'              flavors = c("r-devel-linux-x86_64-debian-clang",
#'              "r-devel-linux-x86_64-debian-gcc"))
pac_checkred <- function(pac, scope = c("ERROR", "FAIL"), flavors = NULL, repos = "https://cran.rstudio.com/") {
  stopifnot(all(scope %in% c("ERROR", "FAIL", "WARN", "NOTE")))
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.character(repos))
  stopifnot(length(scope) == 0 || all(scope %in% c("ERROR", "FAIL", "WARN", "NOTE")) &&
            is.null(flavors) || all(flavors %in% cran_flavors()$Flavor)
  )

  if (!pac %in% rownames(available_packages(repos = repos))) {
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
#' @note Results are cached for 1 hour with `memoise` package.
#' @export
checked_packages <- function() {
  packages <- read_checkred_packages()
  if (is.data.frame(packages)) {
    result <- packages
  } else {
    result <- NA
  }
  result
}

read_checkred_packages_raw <- function() {
  rr <- try(readLines("https://cran.r-project.org/web/checks/check_summary_by_package.html"), silent = TRUE)

  if (!inherits(rr, "try-error")) {
    length_rr <- length(rr)
    rr_range <- grep("</?table[^>]*>", rr)
    rrr <- rr[(rr_range[1] + 1):(rr_range[2] - 1)]
    header <- trimws(xml_text(xml_find_all(read_html(rrr[1]), "//th")))
    header_machines <- trimws(gsub("check_flavors.html#", "", xml_attr(xml_find_all(read_html(rrr[1]), "//th/a"), "href")))
    header[3:(length(header_machines) + 2)] <- header_machines
    length_rrr <- length(rrr)
    result_raw <- matrix(trimws(xml_text(xml_find_all(read_html(paste0(rrr[2:length_rrr], collapse = "\n")), "/html/body//tr/td"))),
                         ncol = length(header),
                         nrow = length_rrr - 1, byrow = TRUE)

    result_raw <- as.data.frame(result_raw)
    colnames(result_raw) <- header
  } else {
    result_raw <- NA
  }
  result_raw
}

read_checkred_packages <- memoise::memoise(read_checkred_packages_raw, cache = cachem::cache_mem(max_age = 60*60))

read_cran_flavours_raw <- function() {
  base_url <- "https://cran.r-project.org/web/checks/check_flavors.html"
  rr <- try(readLines(base_url), silent = TRUE)
  if (!inherits(rr, "try-error")) {
    rr_range <- grep("</?table[^>]*>", rr)
    rrr <- rr[(rr_range[1] + 1):(rr_range[2] - 1)]
    header <- trimws(xml_text(xml_find_all(read_html(rrr[1]), "//th")))

    result_raw <- as.data.frame(matrix(trimws(xml_text(xml_find_all(read_html(paste(rrr[2:length(rrr)], collapse = "\n")), "//td"))),
                                       ncol = length(header),
                                       nrow = length(rrr) - 1, byrow = TRUE))
    colnames(result_raw) <- header
    result_raw$Flavor <- gsub("\\+", "_", result_raw$Flavor)
  } else {
    result_raw <- NA
  }

  result_raw
}

read_cran_flavours <- memoise::memoise(read_cran_flavours_raw)

#' Retrieving all R CRAN servers flavors
#' @description Retrieving all R CRAN servers flavors.
#' The data is downloaded from `https://cran.r-project.org/web/checks/check_flavors.html`.
#' @return data.frame with the same structure as the html table on `https://cran.r-project.org/web/checks/check_flavors.html`.
#' @note Results are cached for 1 hour with `memoise` package.
#' @export
#' @examples
#' cran_flavors()
cran_flavors <- function() {
  read_cran_flavours()
}
