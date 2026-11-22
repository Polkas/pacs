test_that("pacs::pac_description", {
  skip_if_offline()
  expect_true(length(pac_description("dplyr", version = "0.8.0")) == 23)
  expect_true(utils::compareVersion(
    pac_description("memoise", local = TRUE)$Version,
    pac_description("memoise", local = FALSE)$Version
  ) %in% c(0, -1))
  expect_identical(pac_description("dplyr", "10.1.1.1"), NA)
  expect_identical(pac_description("WRONG"), NA)
  expect_identical(pac_description("WRONG", local = TRUE), NA)
  expect_identical(pac_description("dplyr", "0.0.0.1"), NA)
  expect_silent(pac_description("dplyr", version = pac_last("dplyr", repos = "https://cran.rstudio.com/")))
  expect_true(as.Date(pac_description("memoise", at = as.Date("2019-01-01"))[["Date/Publication"]]) <= as.Date("2019-01-01"))
})

test_that("CRAN-only description lookup avoids Bioconductor fallback messages", {
  cran <- c(CRAN = "https://cran.rstudio.com/")
  latest <- pac_last
  mockery::stub(latest, "biocran_repos", function() {
    message("No matching Bioconductor release for R 4.7; using the CRAN repository only.")
    cran
  })
  mockery::stub(latest, "pac_isin", TRUE)
  mockery::stub(latest, "last_version_fun", function(pac, repos) {
    stopifnot(identical(repos, cran))
    "1.0.0"
  })

  description <- pac_description
  mockery::stub(description, "installed_packages", matrix(character(), nrow = 0, ncol = 1))
  mockery::stub(description, "is_online", TRUE)
  mockery::stub(description, "is_isin", TRUE)
  mockery::stub(description, "pac_last", latest)
  mockery::stub(description, "pac_description_dcf", function(pac, version, repos) {
    stopifnot(identical(repos, cran))
    list(Package = pac, Version = version)
  })

  expect_message(
    description("example", version = latest("example"), repos = cran),
    "No matching Bioconductor release for R 4.7"
  )
  expect_silent(
    result <- description("example", version = latest("example", repos = cran), repos = cran)
  )
  expect_identical(result, list(Package = "example", Version = "1.0.0"))
})
