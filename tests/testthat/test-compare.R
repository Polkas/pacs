test_that("pacs::pac_compare_versions", {
  skip_if_offline()
  expect_error(suppressWarnings(pac_compare_versions("memoise", "2.0.0", "22.4.0")))
  expect_error(pac_compare_versions("memoise", "22.8.0", "22.4.0"))
})

test_that("pacs::pac_compare_versions online", {
  skip_if_offline()
  expect_true(nrow(pac_compare_versions("memoise", "0.2.1", "2.0.0")) == 3)
  expect_true(suppressWarnings(any(duplicated(colnames(pac_compare_versions("memoise", "2.0.0", "2.0.0"))))))
})

test_that("pacs::pac_compare_namesapce", {
  expect_error(suppressWarnings(pac_compare_namespace("memoise", "2.0.0", "22.4.0")))
  expect_error(pac_compare_namespace("memoise", "22.8.0", "22.4.0"))
})

test_that("pacs::pac_compare_namesapce online", {
  skip_if_offline()
  expect_true(length(pac_compare_namespace("dplyr", "0.7.1", "1.0.0")) == 10)
  expect_true(length(pac_compare_namespace("shiny", "1.0.0", "1.5.0")) == 10)
  expect_true(length(pac_compare_namespace("memoise", "0.2.1", "2.0.0")) == 10)
  expect_identical(pac_compare_namespace("memoise", "0.2.1", "2.0.0")$exports$added, c(
    "cache_filesystem", "cache_gcs", "cache_memory", "cache_s3",
    "drop_cache", "has_cache", "timeout"
  ))
})
