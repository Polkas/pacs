#' Package version at a specific Date or a Date interval
#' @description using CRAN website to get a package version/versions used at a specific Date or a Date interval.
#' @param pac character a package name.
#' @param at Date old version of package. Default: NULL
#' @param from Date new version of package. Default: NULL
#' @param version character version of package. Default: NULL
#' @param to Date CRAN URL. Default: NULL
#' @param repos character the base URL of the repositories to use. Default `https://cran.rstudio.com/`
#' @return data.frame
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their servers by constantly using this function.
#' The base part of URL in the result is `https://cran.r-project.org/src/contrib/`.
#' Results are cached for 1 hour with `memoise` package.
#' @export
#' @examples
#' pac_timemachine("dplyr", at = as.Date("2017-02-02"))
#' pac_timemachine("dplyr", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))
#' pac_timemachine("dplyr", at = Sys.Date())
pac_timemachine <- function(pac, at = NULL, from = NULL, to = NULL, version = NULL, repos = "https://cran.rstudio.com/") {
  stopifnot(pac %in% c(rownames(available_packages(repos = repos)), pacs_base()))
  stopifnot(xor(
    !is.null(at) && inherits(at, "Date") && is.null(version),
    !is.null(from) && !is.null(to) && from <= to && inherits(from, "Date") && inherits(to, "Date") && is.null(at) && is.null(version)
  ) ||
    all(c(is.null(at), is.null(from), is.null(to), is.null(version))) || (!is.null(version) && length(version) == 1))

  result <- pac_archived(pac)
  cran_page <- pac_cran_recent(pac)

  if (is.null(result)) {
    return(cran_page)
  }

  result$Archived <- as.Date(c(result$Released[-1], cran_page$Released), origin = "1970-01-01")
  result$Life_Duration <- result$Archived - result$Released
  f_cols <- c("Package", "Version", "Released", "Archived", "Life_Duration", "URL", "Size")
  result <- rbind(result[, f_cols], cran_page[, f_cols])
  result <- result[, f_cols]

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
  } else if (!is.null(version)) {
    result[result$Version == version, ]
  } else {
    result
  }
}

pac_cran_recent_raw <- function(pac) {
  cran_page <- try(readLines(sprintf("https://CRAN.R-project.org/package=%s", pac)), silent = TRUE)
  if (!inherits(cran_page, "try-error")) {
    cran_v <- gsub("</?td>", "", cran_page[grep("Version:", cran_page) + 1])
    cran_released <- gsub("</?td>", "", cran_page[grep("Published:", cran_page) + 1])

    data.frame(
      Package = pac,
      Released = cran_released,
      Size = NA,
      Description = NA,
      Version = cran_v,
      Archived = NA,
      Life_Duration = Sys.Date() - as.Date(cran_released),
      URL = sprintf("%s_%s.tar.gz", pac, cran_v),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      Package = pac,
      Released = NA,
      Size = NA,
      Description = NA,
      Version = NA,
      Archived = NA,
      Life_Duration = NA,
      URL = NA,
      stringsAsFactors = FALSE
    )
  }
}

pac_cran_recent <- memoise::memoise(pac_cran_recent_raw, cache = cachem::cache_mem(max_age = 60 * 60))

pac_archived_raw <- function(pac) {
  base_archive <- sprintf("/src/contrib/Archive/%s/", pac)
  rr <- try(readLines(paste0("https://cran.r-project.org/", base_archive)), silent = TRUE)

  if (!inherits(rr, "try-error")) {
    rr_range <- grep("</?table>", rr)
    rrr <- rr[(rr_range[1] + 1):(rr_range[2] - 1)]
    # not use rvest as it is too big dependency
    header <- stringi::stri_match_all(rrr[[1]], regex = ">([^<>]+)<")[[1]][, 2]

    result_raw <- as.data.frame(
      do.call(
        rbind,
        lapply(
          4:(length(rrr) - 1),
          function(x) stringi::stri_match_all(rrr[x], regex = ">([^<>]+)<")[[1]][, 2]
        )
      ),
      stringsAsFactors = FALSE
    )
    colnames(result_raw) <- header

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
    result <- NULL
  }

  result
}

pac_archived <- memoise::memoise(pac_archived_raw, cache = cachem::cache_mem(max_age = 60 * 60))
