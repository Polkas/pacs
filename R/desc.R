pac_description_raw <- function(pac, version = NULL) {

    ee = tempfile(fileext = "tar.gz")

    last_version <- available_packages()[rownames(available_packages()) == pac, "Version"]

    if (!is.null(version) && version != last_version) {
     base_url <- sprintf("https://cran.r-project.org/src/contrib/Archive/%s", pac)
    } else {
     base_url <- "https://cran.r-project.org/src/contrib"
     version <- last_version
    }

    d_url <- sprintf("%s/%s_%s.tar.gz",
                     base_url,
                     pac,
                     version)

    utils::download.file(d_url,
                  destfile = ee,
                  quiet = TRUE)

    tt <- tempdir(check = TRUE)

    utils::untar(ee, exdir = tt)
    # tabs are not acceptable
    yaml::yaml.load(gsub("\t",
                         "  ",
                         paste(readLines(paste0(tt, sprintf("/%s/DESCRIPTION", pac))),
                               collapse = "\n"))
                    )
}

pac_description <- memoise::memoise(pac_description_raw)


pacs_description <- function(pacs, versions = NULL) {
  stopifnot(is.null(versions) || length(pacs) == length(versions))

  stats::setNames(lapply(seq_along(pacs),
                         function(x) pac_description(pacs[x], version = versions[x])),
                  pacs)
}
