test_that("pacs::pac_namespace", {
  skip_if_offline()
  expect_warning(pac_readnamespace_raw("dplyr", "0.0.0.0.1", NULL))
  expect_true(length(pac_namespace("dplyr", version = "0.8.0")) == 10)
  expect_true(length(pac_parse_namespace(readLines("files/NAMESPACE_joint.txt"), enc = "UTF-8")) == 10)
  expect_identical(sort(pac_namespace("memoise", local = TRUE)$exports), sort(base::getNamespaceExports("memoise")))
  expect_identical(suppressWarnings(pac_namespace("dplyr", "1.1.1.1")), structure(list(), package = "dplyr", version = "1.1.1.1"))
  expect_identical(pac_namespace("WRONG"), structure(list(), package = "WRONG"))
  expect_identical(pac_namespace("WRONG", local = TRUE), structure(list(), package = "WRONG"))
  expect_silent(pac_namespace("xml2", at = NULL, version = NULL))
  expect_silent(pac_namespace("xml2", version = NULL, at = as.Date("2021-01-01")))
  expect_identical(suppressWarnings(pac_namespace("dplyr", "0.0.0.1")), structure(list(), package = "dplyr", version = "0.0.0.1"))
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
