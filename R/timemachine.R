#' Package version at a specific Date or a Date interval
#' @description using CRAN website to get a package version/versions used at a specific Date or a Date interval.
#' @param pac character a package name.
#' @param at Date old version of package. Default: NULL
#' @param from Date new version of package. Default: NULL
#' @param version character version of package. Default: NULL
#' @param to Date CRAN URL. Default: NULL
#' @return data.frame with 7 columns
#' \describe{
#' \item{Package}{character package name.}
#' \item{Version}{character package version.}
#' \item{Released}{character release Date}
#' \item{Archived}{character archived Date.}
#' \item{LifeDuration}{`difftime` number of days the version was the newest one.}
#' \item{URL}{character the suffix of the base URL to tar.gz file.}
#' \item{Size}{character size of the `tar.gz` file.}
#' }
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their servers by constantly using this function.
#' The base part of URL in the result is `https://cran.r-project.org/src/contrib/`.
#' Results are cached for 1 hour with `memoise` package.
#' @export
#' @examples
#' \dontrun{
#' pac_timemachine("dplyr", at = as.Date("2017-02-02"))
#' pac_timemachine("dplyr", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))
#' pac_timemachine("dplyr", at = Sys.Date())
#' }
pac_timemachine <- function(pac,
                            at = NULL,
                            from = NULL,
                            to = NULL,
                            version = NULL) {
  stopifnot(pac_isin(pac, "https://cran.rstudio.com/"))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))
  stopifnot(xor(
    !is.null(at) && inherits(at, "Date") && is.null(version),
    !is.null(from) && !is.null(to) && from <= to && inherits(from, "Date") && inherits(to, "Date") && is.null(at) && is.null(version)
  ) ||
    all(c(is.null(at), is.null(from), is.null(to), is.null(version))) || (!is.null(version) && length(version) == 1 && is.character(version)))

  result <- pac_archived(pac)
  cran_page <- pac_cran_recent(pac)

  if (isNA(result)) {
    return(cran_page)
  }

  if (isNA(cran_page)) {
    return(NA)
  }

  result$Archived <- as.Date(c(result$Released[-1], cran_page$Released))
  result$LifeDuration <- result$Archived - result$Released
  f_cols <- c("Package", "Version", "Released", "Archived", "LifeDuration", "URL", "Size")
  result <- rbind(result[, f_cols], cran_page[, f_cols])
  result <- result[, f_cols]

  if (isTRUE(!is.null(at))) {
    if (isTRUE(all(at >= result$Released))) {
      utils::tail(result, 1)
    } else {
      result[at >= result$Released & at <= result$Archived, ]
    }
  } else if (isTRUE(!is.null(from) && !is.null(to))) {
    if (all(from >= result$Released)) {
      utils::tail(result, 1)
    } else {
      result[to >= result$Released & from <= result$Archived, ]
    }
  } else if (isTRUE(!is.null(version))) {
    result[result$Version == version, ]
  } else {
    result
  }
}

pac_cran_recent_raw <- function(pac) {
  cran_page <- try(suppressWarnings(readLines(sprintf("https://CRAN.R-project.org/package=%s", pac), warn = FALSE)), silent = TRUE)
  if (!inherits(cran_page, "try-error") && any(grepl(pac, cran_page))) {
    cran_v <- utils::head(gsub("</?td>", "", cran_page[grep("Version:", cran_page) + 1]), 1)
    cran_released <- utils::head(gsub("</?td>", "", cran_page[grep("Published:", cran_page) + 1]), 1)

    if (length(cran_v) == 0) cran_v <- NA
    if (length(cran_released) == 0) cran_released <- NA
    f_cols <- c("Package", "Version", "Released", "Archived", "LifeDuration", "URL", "Size")

    data.frame(
      Package = pac,
      Version = cran_v,
      Released = as.Date(cran_released),
      Archived = NA,
      LifeDuration = Sys.Date() - as.Date(cran_released),
      URL = sprintf("%s_%s.tar.gz", pac, cran_v),
      Size = NA,
      stringsAsFactors = FALSE
    )
  } else {
    NA
  }
}

pac_cran_recent <- memoise::memoise(pac_cran_recent_raw, cache = cachem::cache_mem(max_age = 60 * 60))

pac_archived_raw <- function(pac) {
  base_archive <- sprintf("/src/contrib/Archive/%s/", pac)
  rr <- try(suppressWarnings(readLines(paste0("https://cran.r-project.org", base_archive), warn = FALSE)), silent = TRUE)

  if (!inherits(rr, "try-error") && any(grepl(pac, rr))) {
    rr_range <- grep("</?table>", rr)
    if (length(rr_range) != 2) {
      return(NA)
    }
    rrr <- rr[(rr_range[1] + 1):(rr_range[2] - 1)]
    header <- trimws(xml_text(xml_find_all(read_html(rrr[1]), "//th")))

    result_raw <- as.data.frame(matrix(trimws(xml_text(xml_find_all(read_html(paste(rrr[2:length(rrr)], collapse = "\n")), "//td"))),
      ncol = length(header),
      nrow = length(rrr) - 3, byrow = TRUE
    ))
    result_raw <- result_raw[-1, -1]
    colnames(result_raw) <- header[-1]

    result <- result_raw[result_raw[["Last modified"]] != "", ]
    colnames(result) <- c("Package", "Released", "Size", "Description")
    result$Released <- as.Date(result$Released)

    pac_raw <- strsplit(gsub(".tar.gz", "", result$Package), "_")
    pac_name <- vapply(pac_raw, function(x) x[1], character(1))
    pac_v <- vapply(pac_raw, function(x) x[2], character(1))
    result$URL <- paste0(sprintf("Archive/%s/", pac), result$Package)
    result$Package <- pac_name
    result$Version <- pac_v
    result <- result[order(result$Released), ]
  } else {
    result <- NA
  }

  result
}

pac_archived <- memoise::memoise(pac_archived_raw, cache = cachem::cache_mem(max_age = 60 * 60))
