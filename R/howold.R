#' Package version at a specific Date or a Date interval
#' @description using cran website to get a package version/versions used at a specific Date or a Date interval.
#' @param pac character a package name.
#' @param at Date old version of package. Default: NULL
#' @param from Date new version of package. Default: NULL
#' @param to Date cran URL. Default: NULL
#' @return data.frame
#' @note Function will scrap two CRAN URLS.
#' @export
#' @examples
#' \dontrun{
#' pac_versions("dplyr", at = as.Date("2017-02-02"))
#' pac_versions("dplyr", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))
#' pac_versions("dplyr", at = Sys.Date())
#' }
pac_versions <- function(pac , at = NULL, from = NULL, to = NULL) {
  stopifnot(pac %in% rownames(utils::available.packages(repos = "http://cran.rstudio.com/")))
  stopifnot(xor(!is.null(at) && inherits(at, "Date"),
                !is.null(from) && !is.null(to) && from <= to && inherits(from, "Date") && inherits(to, "Date")) ||
              all(c(is.null(at), is.null(from), is.null(to))))

  rr <- readLines(sprintf("https://cran.r-project.org/src/contrib/Archive/%s/", pac))
  rr_range <- grep("table", rr)
  rrr <- rr[(rr_range[1] + 1):(rr_range[2] - 1)]
  # not use rvest as it is too big dependency
  header <- unlist(lapply(xml2::xml_find_all(xml2::read_html(rrr[1]), "//th"), xml2::xml_text))

  result_raw <- as.data.frame(
    do.call(
      rbind,
      lapply(4:length(rrr), function(x) {
        unlist(lapply(xml2::xml_find_all(xml2::read_html(rrr[x]), "//td"), xml2::xml_text))
      })
    ),
    stringsAsFactors = FALSE
  )[, -1]
  colnames(result_raw) <- header[-1]

  result <- result_raw[result_raw[["Last modified"]] != "", ]
  colnames(result) <- c("Package", "Released", "Size", "Description")
  result$Released <- as.Date(result$Released)

  pac_raw <- strsplit(gsub(".tar.gz", "", result$Package), "_")
  pac_name <- vapply(pac_raw, function(x) x[1], character(1))
  pac_v <- vapply(pac_raw, function(x) x[2], character(1))
  result$Package <- pac_name
  result$Version <- pac_v
  result <- result[order(result$Released), ]

  cran_page <- readLines(sprintf("https://CRAN.R-project.org/package=%s", pac))
  cran_page_paste <- paste(cran_page, collapse = "\n")
  cran_list <- unlist(lapply(xml2::xml_find_all(xml2::read_html(cran_page_paste),
                     "/html/body//td"), xml2::xml_text))
  cran_v <- cran_list[grep("Version", cran_list) + 1]
  cran_released <- cran_list[grep("Published", cran_list) + 1]

  result$Archived <- as.Date(c(result$Released[-1], cran_released), origin = "1970-01-01")
  result <- rbind(result, data.frame(Package = pac, Released = cran_released, Size = NA, Description = NA, Version = cran_v, Archived = NA, stringsAsFactors = FALSE))
  result$Life_Duration <- result$Archived - result$Released
  result <- result[, c("Package", "Version", "Released", "Archived", "Life_Duration", "Size", "Description")]

  if (!is.null(at)) {
    if (all(at >= result$Released)) {
      utils::tail(result, 1)
    } else {
      result[at >= result$Released & at <= result$Archived, ]
    }
  } else if (!is.null(from) && !is.null(to)) {
    if (all(from >= result$Released)) {
      utils::tail(result, 1)
    } else {
      result[to >= result$Released & from <= result$Archived, ]
    }
  } else {
    result
  }
}

#' Packages versions at a specific Date or a Date interval
#' @description using cran website to get packages version/versions used at a specific Date or a Date interval.
#' @param pacs character vector packages names.
#' @param at Date old version of package. Default: NULL
#' @param from Date new version of package. Default: NULL
#' @param to Date cran URL. Default: NULL
#' @return data.frame
#' @note Function will scrap two CRAN URLS.
#' @export
#' @examples
#' \dontrun{
#' pacs_versions(c("dplyr", "shiny"), from = as.Date("2018-06-30"), to = as.Date("2019-01-01"))
#' pacs_versions(c("dplyr", "shiny"), at = Sys.Date())
#' }
pacs_versions <- function(pacs, at = NULL, from = NULL, to = NULL) {
  stopifnot(all(pacs %in% rownames(utils::available.packages(repos = "http://cran.rstudio.com/"))))
  stats::setNames(lapply(pacs, function(pac) pac_versions(pac, at, from, to)), pacs)
}
