test_that("Bioconductor configuration provides released versions and dates", {
  result <- read_bio_releases_raw("files/bioconductor_config.yaml")

  expect_identical(names(result), c("Release", "Date", "Software packages", "R"))
  expect_identical(result$Release, c("3.23", "3.22", "3.21", "3.10", "3.9", "1.6", "1.0"))
  expect_identical(result$Date[1], as.Date("2026-04-29"))
  expect_identical(result$R, c("4.6", "4.5", "4.5", "3.6", "3.6", "2.1", NA_character_))
  expect_true(all(is.na(result[["Software packages"]])))
  expect_false("3.24" %in% result$Release)
})

test_that("unavailable or invalid Bioconductor configurations are reported", {
  expect_error(read_bio_releases_raw("files/does-not-exist.yaml"))

  invalid <- tempfile(fileext = ".yaml")
  on.exit(unlink(invalid), add = TRUE)
  writeLines(c('release_version: "3.0"', "release_dates:", '  "3.0": "not-a-date"', "r_ver_for_bioc_ver:", '  "3.0": "3.1"'), invalid)
  expect_error(read_bio_releases_raw(invalid), "release dates are invalid")

  writeLines(c('release_version: "3.1"', "release_dates:", '  "3.0": "10/14/2014"', "r_ver_for_bioc_ver:", '  "3.0": "3.1"'), invalid)
  expect_error(read_bio_releases_raw(invalid), "current release date is missing")

  releases <- bio_releases
  mockery::stub(releases, "read_bio_releases", function() stop("temporary failure"))
  expect_message(
    expect_identical(releases(), NA),
    "Failed to read Bioconductor release configuration: temporary failure"
  )
})

test_that("a temporary Bioconductor configuration failure is not cached", {
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)
  expect_error(read_bio_releases(path))
  expect_true(file.copy("files/bioconductor_config.yaml", path))
  expect_s3_class(read_bio_releases(path), "data.frame")
})

test_that("Bioconductor repositories select released versions matching R", {
  r_version <- paste0(R.Version()$major, ".", strsplit(R.Version()$minor, ".", fixed = TRUE)[[1]][1])
  releases <- data.frame(
    Release = c("3.13", "3.12", "3.11", "3.10"),
    R = c(r_version, r_version, r_version, "0.0"),
    stringsAsFactors = FALSE
  )
  repos <- biocran_repos
  mockery::stub(repos, "is_online", TRUE)
  mockery::stub(repos, "bio_releases", releases)

  expect_identical(unname(repos()["BioCsoft"]), "https://bioconductor.org/packages/3.13/bioc")
  expect_identical(unname(repos("3.12")["BioCsoft"]), "https://bioconductor.org/packages/3.12/bioc")
  expect_true("BioCbooks" %in% names(repos("3.12")))
  expect_false("BioCbooks" %in% names(repos("3.11")))
  expect_message(
    expect_identical(repos("3.10"), c(CRAN = "https://cran.rstudio.com/")),
    "No matching Bioconductor release"
  )
  mockery::stub(repos, "bio_releases", NA)
  expect_message(
    expect_identical(repos(), c(CRAN = "https://cran.rstudio.com/")),
    "release information is unavailable"
  )
  mockery::stub(repos, "is_online", FALSE)
  expect_message(
    expect_identical(repos(), c(CRAN = "https://cran.rstudio.com/")),
    "No internet connection"
  )
})
