#' Get NEWS for a package
#' @description Get NEWS for a package from CRAN or local
#' @inheritParams standard_args
#' @param repos `character` vector repositories URLs to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return `character` with NEWS content.
#' @note Results are cached for 30 minutes with `memoise` package.
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_news("dplyr", version = "0.8.0")
#' pacs::pac_news("dplyr", at = as.Date("2019-02-01"))
#' }
pac_news <- function(
    pac,
    version = NULL,
    at = NULL,
    local = FALSE,
    lib.loc = .libPaths(),
    repos = "https://cran.rstudio.com/"
) {

    validate_pac_input(pac, version, at, local, lib.loc, repos)

    is_installed <- isTRUE(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
    if ((!is_installed && local) || (!local && !is_online())) {
        return(NA)
    }

    version_installed <- if (is_installed) {
        utils::packageDescription(pac, lib.loc = lib.loc)$Version
    } else {
        NA
    }
    version_null <- is.null(version)

    if (local && is_installed && is.null(at) && (version_null || isTRUE(utils::compareVersion(version, version_installed) == 0))) {
        package_dir <- find.package(pac, lib.loc = lib.loc)
        news_name <- intersect(list.files(package_dir), c("NEWS.md", "NEWS", "NEWS.Rmd"))
        if (length(news_name) == 0) {
            return(NA)
        }
        return(readLines(file.path(package_dir, news_name[1]), warn = FALSE))
    } else if (isTRUE(is_isin(pac, "https://cran.rstudio.com/"))) {
        last_version <- pac_last(pac, repos = repos)
        version <- if (!version_null) {
            version
        } else if (!is.null(at)) {
            history <- pac_timemachine(pac, at = at)
            if (!is.data.frame(history) || nrow(history) == 0) {
                return(NA)
            }
            utils::tail(history$Version, 1)
        } else {
            last_version
        }
        news_lines <- pac_readnews(pac, version, repos)
        if (isTRUE(is.na(news_lines))) {
            return(NA)
        } else if (length(news_lines) == 0) {
            return(NA)
        } else {
            return(news_lines)
        }
    } else {
        return(NA)
    }
}


pac_readnews_raw <- function(pac, version, repos = "https://cran.rstudio.com/") {
    ee <- tempfile()
    d_url <- sprintf(
        "https://raw.githubusercontent.com/cran/%s/%s/NEWS",
        pac,
        version
    )
    tt <- try(
    {
      suppressWarnings(utils::download.file(d_url,
        destfile = ee,
        quiet = TRUE
      ))
    },
    silent = TRUE
  )
  if (inherits(tt, "try-error")) {
    result <- read_cran_file(pac, version, "NEWS", repos)
  } else {
    result <- readLines(ee)
    unlink(ee)
  }
  result
}

pac_readnews <- memoise::memoise(pac_readnews_raw, cache = cachem::cache_mem(max_age = 30 * 60))
