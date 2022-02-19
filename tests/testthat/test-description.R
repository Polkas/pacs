test_that("cran_archive_description", {
  skip_if_offline()
  expect_true(length(cran_archive_description("dplyr", "1.0.0", "https://cran.rstudio.com/")) == 22)
  expect_identical(cran_archive_description("dplyr", "0.0.0.1", "https://cran.rstudio.com/"),
                   structure(list(), package = "dplyr", version = "0.0.0.1"))
})

test_that("pacs::pac_description", {
  skip_if_offline()
  expect_true(length(pac_description("dplyr", version = "0.8.0")) == 23)
  expect_true(utils::compareVersion(
    pac_description("memoise", local = TRUE)$Version,
    pac_description("memoise", local = FALSE)$Version
  ) %in% c(0, -1))
  expect_identical(suppressWarnings(pac_description("dplyr", "1.1.1.1")), structure(list(), package = "dplyr", version = "1.1.1.1"))
  expect_identical(pac_description("WRONG"), structure(list(), package = "WRONG"))
  expect_identical(suppressWarnings(pac_description("dplyr", "0.0.0.1")), structure(list(), package = "dplyr", version = "0.0.0.1"))
  expect_silent(pac_description("dplyr", version = pac_last("dplyr")))
})
