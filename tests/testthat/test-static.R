test_that("pacs::checked_packages", {
  skip_if_offline()
  expect_true(
    isNA(checked_packages()) || (is.data.frame(checked_packages()) &&
      (nrow(checked_packages()) > 0) &&
      all(c("Package", "Version", "Maintainer", "Priority") %in% colnames(checked_packages())))
  )
})

test_that("checked_packages offline", {
  checked_packages_offline <- checked_packages
  mockery::stub(checked_packages_offline, "is_online", FALSE)
  expect_message(checked_packages_offline(), "No internet connection detected")
  expect_identical(suppressMessages(checked_packages_offline()), NA)
})

test_that("pacs::cran_flavors()", {
  skip_if_offline()
  skip_if(isNA(checked_packages()))
  expect_true(
    isNA(cran_flavors()) || (is.data.frame(cran_flavors()) &&
      any(cran_flavors()$Flavor %in% colnames(checked_packages())) &&
      (nrow(cran_flavors()) > 0))
  )
})

test_that("pacs::pac_checkpage", {
  skip_if_offline()
  expect_true(
    isNA(pac_checkpage("dplyr")) || (is.data.frame(pac_checkpage("dplyr")) &&
      (nrow(pac_checkpage("dplyr")) > 0) &&
      any(pac_checkpage("dplyr")$Flavor %in% cran_flavors()$Flavor))
  )
  expect_message(pac_checkpage("WRONG"), "WRONG package is not on CRAN")
  expect_true(isNA(suppressMessages(pac_checkpage("WRONG"))))
})

test_that("pacs::pac_checkpage offline", {
  pac_checkpage_offline <- pac_checkpage
  mockery::stub(pac_checkpage_offline, "is_online", FALSE)
  expect_message(pac_checkpage_offline("dplyr"), "No internet connection detected")
  expect_identical(suppressMessages(pac_checkpage_offline("dplyr")), NA)
})


test_that("pacs::bio_releases()", {
  skip_if_offline()
  expect_true(
    isNA(bio_releases()) || is.data.frame(bio_releases()) &&
      (nrow(bio_releases()) > 0) &&
      all(colnames(bio_releases()) %in% c("Release", "Date", "Software packages", "R"))
  )
})

test_that("pacs::biocran_repos()", {
  expect_identical(biocran_repos("4.3.3.3.3"), c(CRAN = "https://cran.rstudio.com/"))
})

test_that("pacs::biocran_repos() online", {
  skip_if_offline()
  expect_true(length(biocran_repos()) > 0)
})

test_that("pacs::biocran_repos() offline", {
  biocran_repos_offline <- biocran_repos
  mockery::stub(biocran_repos_offline, "is_online", FALSE)
  expect_identical(biocran_repos_offline(), c(CRAN = "https://cran.rstudio.com/"))
})

test_that("biocran_repos", {
  expect_identical(biocran_repos(NA), c(CRAN = "https://cran.rstudio.com/"))
})

test_that("match_flavors offline", {
  match_flavors_offline <- match_flavors
  mockery::stub(match_flavors_offline, "is_online", FALSE)
  expect_message(match_flavors_offline(), "No internet connection detected")
  expect_true(isNA(suppressMessages(match_flavors_offline())))
})

test_that("match_flavors", {
  skip_if_offline()
  expect_true(length(match_flavors()) > 0)
})
