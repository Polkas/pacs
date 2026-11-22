#' Read a file from CRAN
#' @description Read a file from CRAN package source.
#' @inheritParams standard_args
#' @keywords internal
read_cran_file <- function(pac, version, file, repos = "https://cran.rstudio.com/") {
  stopifnot(is_online())

  last_version <- pac_last(pac, repos)
  if (length(last_version) != 1L || is.na(last_version)) {
    return(NA)
  }

  if (isTRUE(!is.null(version) && version != last_version)) {
    base_url <- sprintf("https://cran.r-project.org/src/contrib/Archive/%s", pac)
  } else {
    base_url <- "https://cran.r-project.org/src/contrib"
    version <- last_version
  }

  d_url <- sprintf(
    "%s/%s_%s.tar.gz",
    base_url,
    pac,
    version
  )

  temp_tar <- tempfile(fileext = ".tar.gz")

  download <- try(
    expr = {
      suppressWarnings(utils::download.file(d_url,
        destfile = temp_tar,
        quiet = TRUE
      ))
    },
    silent = TRUE
  )

  if (inherits(download, "try-error")) {
    result <- NA
  } else {
    temp_dir <- tempdir()
    utils::untar(temp_tar, exdir = temp_dir)
    if (file == "DESCRIPTION") {
      result <- as.list(read.dcf(file.path(temp_dir, pac, "DESCRIPTION"))[1, ])
    } else if (file == "NAMESPACE") {
      result <- readLines(file.path(temp_dir, pac, "NAMESPACE"), warn = FALSE)
    } else if (file == "NEWS") {
      news_name <- intersect(list.files(file.path(temp_dir, pac)), c("NEWS.md", "NEWS", "NEWS.Rmd"))
      if (length(news_name) == 0) {
        warning("NEWS file not found")
        return(NA)
      }
      result <- readLines(file.path(temp_dir, pac, news_name[1]), warn = FALSE)
    } else {
      stop("Invalid file name")
    }
  }
  unlink(temp_tar)
  result
}
