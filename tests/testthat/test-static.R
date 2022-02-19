test_that("pacs::checked_packages", {
  skip_if_offline()
  expect_true(is.data.frame(checked_packages()) &&
    (nrow(checked_packages()) > 0) &&
    all(c("Package", "Version", "Maintainer", "Priority") %in% colnames(checked_packages())))
})

test_that("pacs::cran_flavors()", {
  skip_if_offline()
  expect_true(is.data.frame(cran_flavors()) &&
    any(cran_flavors()$Flavor %in% colnames(checked_packages())) &&
    (nrow(cran_flavors()) > 0))
})

test_that("pacs::pac_checkpage", {
  skip_if_offline()
  expect_true(is.data.frame(pac_checkpage("dplyr")) &&
    (nrow(pac_checkpage("dplyr")) > 0) &&
    any(pac_checkpage("dplyr")$Flavor %in% cran_flavors()$Flavor))
})

test_that("pacs::bio_releases()", {
  skip_if_offline()
  expect_true(is.data.frame(bio_releases()) &&
    (nrow(bio_releases()) > 0) &&
    all(colnames(bio_releases()) %in% c("Release", "Date", "Software packages", "R")))
})

test_that("pacs::biocran_repos()", {
  expect_identical(biocran_repos("4.3.3.3.3"), c(CRAN = "https://cran.rstudio.com/"))
})

test_that("pacs::biocran_repos() online", {
  skip_if_offline()
  expect_true(length(biocran_repos()) > 0)
})