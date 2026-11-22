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

test_that("app_size adds the app directory once", {
  size <- app_size
  mockery::stub(size, "app_deps", data.frame(Package = c("first", "second")))
  mockery::stub(size, "pac_size", function(pac, lib.loc) {
    stopifnot(identical(lib.loc, .libPaths()))
    if (pac == "first") 10 else 20
  })
  mockery::stub(size, "dir_size", 100)

  expect_equal(size("."), 130)
  mockery::stub(size, "app_deps", data.frame(Package = character()))
  expect_equal(size("."), 100)
  mockery::stub(size, "app_deps", NA)
  expect_message(expect_identical(size("."), NA_real_), "Failed to determine app dependencies")
})
