last_version_raw <- function(pac, repos) {
  which_p <- rownames(available_packages(repos = repos)) == pac
  if (any(which_p)) {
    available_packages(repos = repos)[which_p, "Version"]
  } else {
    NA
  }
}

last_version_fun <- memoise::memoise(last_version_raw, cache = cachem::cache_mem(max_age = 30 * 60))

#' Getting the most recent package version
#' @description using `utils::available.packages` to get the newest package version.
#' @inheritParams standard_args
#' @param repos character vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R Version. Default `pacs::biocran_repos()`
#' @return `character` most recent package version.
#' @note Results are cached for 30 minutes with `memoise` package.
#' For Bioconductor the newest one per R version.
#' @export
#' @examples
#' \dontrun{
#' pac_last("dplyr")
#' pac_last("S4Vectors")
#' }
pac_last <- function(pac, repos = biocran_repos()) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.character(repos))

  if (!pac_isin(pac, repos = repos)) {
    return(NA)
  }

  last_version_fun(pac, repos = repos)
}

#' Checking if a package is in repositories
#' @description using `utils::available.packages` to check if package is in repositories.
#' @inheritParams standard_args
#' @param repos character vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' @return `logical` if a package is inside repositories.
#' @note Results are cached for 30 minutes with `memoise` package.
#' @export
#' @examples
#' \dontrun{
#' pac_isin("dplyr")
#' pac_isin("dplyr", repos = "https://cran.rstudio.com/")
#' pac_isin("dplyr", repos = biocran_repos()[grep("Bio", names(biocran_repos()))])
#' }
pac_isin <- function(pac, repos = biocran_repos()) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.character(repos))

  is_isin(pac, repos = repos)
}

is_isin_raw <- function(pac, repos = biocran_repos()) {
  if (isTRUE(pac %in% rownames(available_packages(repos = repos)))) {
    TRUE
  } else {
    FALSE
  }
}

is_isin <- memoise::memoise(is_isin_raw, cache = cachem::cache_mem(max_age = 30 * 60))

#' Checking if a package version is the most recent one
#' @description checking if a package version is the most recent one, by default the installed version is compared.
#' @inheritParams standard_args
#' @param repos `character` vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R Version. Default `pacs::biocran_repos()`
#' @return `logical` if a package is inside repositories.
#' @note Results are cached for 30 minutes with `memoise` package.
#' @export
#' @examples
#' \dontrun{
#' pac_islast("memoise")
#' pac_islast("dplyr", version = "1.0.0")
#' pac_islast("S4Vectors")
#' pac_islast("S4Vectors", version = pac_last("S4Vectors"))
#' }
pac_islast <- function(pac, version = NULL, lib.loc = .libPaths(), repos = biocran_repos()) {
  if (isFALSE(pac_isin(pac, repos))) {
    return(FALSE)
  }

  last_version <- pac_last(pac, repos = repos)

  is_installed <- isTRUE(pac %in% rownames(installed_packages(lib.loc = lib.loc)))

  if (isTRUE(is.null(version))) {
    if (is_installed) {
      version <- pac_description(pac, local = TRUE)$Version
    } else {
      return(FALSE)
    }
  }

  isTRUE(utils::compareVersion(last_version, version) == 0)
}
