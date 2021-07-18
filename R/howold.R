#' Package life duration at specific Date or for a specific version
#' @description using cran website to get a package version/versions used at a specific Date interval.
#' @param pac character a package name.
#' @param version character version of package. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @return logical if package is healthy.
#' If the newest release is published less than 7 days ago then the class of object is "not-sure".
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' \dontrun{
#' pac_lifeduration("dplyr")
#' pac_lifeduration("dplyr", version = "0.8.0")
#' }
pac_lifeduration <- function(pac, version = NULL, at = NULL) {
  stopifnot(length(pac) == 1)
  stopifnot(!all(c(!is.null(version), !is.null(at))))

  if (!pac %in% rownames(available_packages)) {
    return(NA)
  }

  last_version <- available_packages[rownames(available_packages) == pac, "Version"]

  if ((is.null(version) && is.null(at)) ||
      (!is.null(version) && isTRUE(utils::compareVersion(version, last_version) == 0))) {
      descr <- utils::packageDescription(pac)
    if (isTRUE(utils::compareVersion(last_version, descr[["Version"]]) == 0)) {
      life <- Sys.Date() - as.Date(descr[["Date/Publication"]])
      return(life)
    } else {
      life <- Sys.Date() - as.Date(pac_description(pac, version = descr[["Version"]])[["Date/Publication"]])
      return(life)
    }
  }

  if (is.null(version)) {
    pac_tm <- utils::tail(pac_timemachine(pac, at = at), 1)
    pac_tm$Life_Duration
  } else {
    pac_tm <- pac_timemachine(pac)
    stopifnot(version %in% pac_tm$Version)
    pac_tm <- pac_tm[pac_tm$Version == version, ]
    pac_tm$Life_Duration
  }
}

#' Package healty state at a specific Date or for a specific version
#' @description using cran website to get a package version/versions used at a specific Date interval.
#' A healthy package is a if it was published for more than 7 days.
#' CRAN team gives around one week to resolved a package which gave errors under the check page.
#' @param pac character a package name.
#' @param version character version of package. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @param limit numeric at least dyas to treat as healthy. Default: 7
#' @return logical if package is healthy.
#' If the newest release is published less than 7 days ago then the class of object is "not-sure".
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' \dontrun{
#' pac_health("dplyr")
#' pac_health("dplyr", version = "0.8.0")
#' }
pac_health <- function(pac, version = NULL, at = NULL, limit = 7) {
  stopifnot(length(pac) == 1)
  stopifnot(!all(c(!is.null(version), !is.null(at))))

  if (!pac %in% rownames(available_packages)) {
    return(NA)
  }

  life <- pac_lifeduration(pac, version = version, at = at)

  res <- life >= limit

  last_version <- available_packages[rownames(available_packages) == pac, "Version"]

  if (is_last_release(pac, version, at) && !res) {
    cat(sprintf("This is a newest release of %s published less than 7 days ago so not sure about score.", pac))
    structure(res, class = "not-sure")
  } else {
    structure(res, class = "sure")
  }
}

#' Packages health state at a specific Date or for a specific versions
#' @description using cran website to get a package version/versions used at a specific Date interval.
#' A healthy package is a if it was published for more than 7 days.
#' CRAN team gives around one week to resolved a package which gave errors under the check page.
#' @param pacs character vector packages names.
#' @param versions character vector versions of packages. Default: NULL
#' @param at Date old version of package. Default: NULL
#' @return logical vector, TRUE if a package is healthy.
#' If the newest release is published less than 7 days ago then the class of object is "not-sure" for newest version.
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' @export
#' @examples
#' \dontrun{
#' pacs_health(c("dplyr", "devtools"))
#' pacs_health(c("dplyr", "devtools"), versions = c("0.8.0", "2.4.0"))
#' }
pacs_health <- function(pacs, versions = NULL, at = NULL) {
  stopifnot(!all(c(!is.null(versions), !is.null(at))))
  stopifnot(is.null(versions) || length(pacs) == length(versions))

  stats::setNames(
    lapply(
      seq_along(pacs),
      function(x) pac_health(pacs[x], version = versions[x], at = at)
    ),
    pacs
  )
}
#' Package version at a specific Date or a Date interval
#' @description using cran website to get a package version/versions used at a specific Date or a Date interval.
#' @param pac character a package name.
#' @param at Date old version of package. Default: NULL
#' @param from Date new version of package. Default: NULL
#' @param version character version of package. Default: NULL
#' @param to Date cran URL. Default: NULL
#' @return data.frame
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' The base part of URL in the result is "https://cran.r-project.org".
#' @export
#' @examples
#' \dontrun{
#' pac_timemachine("dplyr", at = as.Date("2017-02-02"))
#' pac_timemachine("dplyr", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))
#' pac_timemachine("dplyr", at = Sys.Date())
#' }
pac_timemachine <- function(pac, at = NULL, from = NULL, to = NULL, version = NULL) {
  stopifnot(pac %in% c(rownames(available_packages), pacs_base()))
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
  f_cols <- c("Package", "Version", "Released", "Archived", "Life_Duration", "URL", "Size", "Description")
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

#' Packages versions at a specific Date or a Date interval
#' @description using cran website to get packages version/versions used at a specific Date or a Date interval.
#' @param pacs character vector packages names.
#' @param at Date old version of package. Default: NULL
#' @param from Date new version of package. Default: NULL
#' @param to Date cran URL. Default: NULL
#' @return data.frame
#' @note Function will scrap two CRAN URLS. Works only with CRAN packages.
#' For bigger lists might need a few minutes.
#' Please as a courtesy to the R CRAN, don't overload their server by constantly using this function.
#' The base part of URL in the result is "https://cran.r-project.org".
#' @export
#' @examples
#' \dontrun{
#' pacs_timemachine(c("dplyr", "shiny"), from = as.Date("2018-06-30"), to = as.Date("2019-01-01"))
#' pacs_timemachine(c("dplyr", "shiny"), at = Sys.Date())
#' }
pacs_timemachine <- function(pacs, at = NULL, from = NULL, to = NULL) {
  pacs_cran <- intersect(pacs, rownames(available_packages))
  pacs_skip <- setdiff(pacs, pacs_cran)
  if (length(pacs_skip)) cat("Skipping non CRAN packages", pacs_skip, "\n")
  stats::setNames(lapply(pacs_cran, function(pac) pac_timemachine(pac, at, from, to)), pacs_cran)
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
      URL = sprintf("/src/contrib/%s_%s.tar.gz", pac, cran_v),
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

pac_cran_recent <- memoise::memoise(pac_cran_recent_raw)

pac_archived_raw <- function(pac) {
  base_archive <- sprintf("/src/contrib/Archive/%s/", pac)
  rr <- try(readLines(paste0("https://cran.r-project.org/", base_archive)), silent = TRUE)

  if (!inherits(rr, "try-error")) {
    rr_range <- grep("</?table>", rr)
    rrr <- rr[(rr_range[1] + 1):(rr_range[2] - 1)]
    # not use rvest as it is too big dependency
    header <- regmatches(rrr[[1]], gregexec(">([^<>]+)<", rrr[[1]]))[[1]][2, ]

    result_raw <- as.data.frame(
      do.call(
        rbind,
        lapply(
          4:(length(rrr) - 1),
          function(x) regmatches(rrr[x], gregexec(">([^<>]+)<", rrr[x]))[[1]][2, ]
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
    result$URL <- paste0(base_archive, result$Package)
    result$Package <- pac_name
    result$Version <- pac_v
    result <- result[order(result$Released), ]
  } else {
    result <- NULL
  }

  result
}

pac_archived <- memoise::memoise(pac_archived_raw)
