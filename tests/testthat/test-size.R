test_that("pacs::dir_size", {
  current_dir <- dir_size(".")
  expect_true(current_dir > 0)
})

test_that("pacs::pac_size", {
  expect_true(pacs::pac_size("stats") > 0)
  expect_error(pac_size("WRONG"))
})

test_that("pacs::pac_true_size", {
  stats_size <- pacs::pac_true_size("stats") / 10**6
  expect_true(stats_size > 1)
  stats_size2 <- pacs::pac_true_size("stats", exclude_joint = 1L)
  expect_equal(stats_size2, pac_size("stats"))
  expect_error(pac_true_size("WRONG"))
})

test_that("pacs::app_size", {
  skip_if_offline()
  expect_true(pacs::app_size("files/shiny_app") > 0)
  expect_true(pacs::app_size("files/shiny_app_empty") > 0)
  expect_error(pacs::app_size("WRONG"))
})
