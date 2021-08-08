is_red_check_raw <- function(pac, scope = c("ERROR", "WARN"), repos = "https://cran.rstudio.com/") {
  if (!pac %in% rownames(available_packages(repos = repos))) {
    return(NA)
  }
  any(grepl(sprintf("Result: (?:%s)", paste(scope, collapse = "|")),
            get_cran_check_page(pac),
            perl = TRUE
  ))
}

get_cran_check_page_raw <- function(pac) {
  tryCatch(
    {
      readLines(sprintf("https://cran.r-project.org/web/checks/check_results_%s.html", pac))
    },
    error = function(e) NA
  )
}

get_cran_check_page <- memoise::memoise(get_cran_check_page_raw, cache = cachem::cache_mem(max_age = 60*60))

#' Checking the R CRAN package check page status
#' @description using R CRAN package check page to validate if there are ANY errors and/or warnings and/or notes.
#' @param pac character a package name.
#' @param scope character vector scope of the check, accepted values c("ERROR", "FAIL", "WARN", "NOTE"). Default c("ERROR", "FAIL")
#' @param repos character the base URL of the repositories to use. Default `https://cran.rstudio.com/`
#' @return logical if the package fail under specified criteria.
#' @note Results are cached for 1 hour with `memoise` package.
#' If you need to check many packages at once then recommended usage of `pacs::checkpage_packages`.
#' @export
#' @examples
#' pac_checkred("dplyr")
#' pac_checkred("dplyr", scope = c("ERROR"))
pac_checkred <- function(pac, scope = c("ERROR", "FAIL"), repos = "https://cran.rstudio.com/") {
  stopifnot(all(scope %in% c("ERROR", "FAIL", "WARN", "NOTE")))
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.character(repos))

  is_red_check_raw(pac, scope, repos = repos)
}

#' Downloading all R CRAN packages check page statuses.
#' @description retrieving all R CRAN packages check pages.
#' The data is downloaded from `https://cran.r-project.org/web/checks/check_summary_by_package.html`.
#' @return data.frame
#' @note Results are cached for 1 hour with `memoise` package.
#' @export
checkpage_packages <- function() {
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
    rr_range <- grep("</?table[^>]*>", rr)
    rrr <- rr[(rr_range[1] + 1):(rr_range[2] - 1)]
    # not use rvest as it is too big dependency
    header <- trimws(xml2::xml_text(xml2::xml_find_all(xml2::read_html(rrr[1]), "//th")))

    result_raw <- matrix(trimws(xml2::xml_text(xml2::xml_find_all(xml2::read_html(paste(rrr[2:length(rrr)], collapse = "\n")), "//td"))),
                         ncol = length(header),
                         nrow = length(rrr) - 1, byrow = TRUE)

    result_raw <- as.data.frame(result_raw)
    colnames(result_raw) <- header

    result_raw$unique_status <- apply(result_raw[, grep("r-", colnames(result_raw))], 1, function(x) paste0(unique(x), collapse = ", "))

  } else {
    result_raw <- NA
  }
  result_raw
}

read_checkred_packages <- memoise::memoise(read_checkred_packages_raw, cache = cachem::cache_mem(max_age = 60*60))
