test_that("replaceNA", {
  expect_true(all(!is.na(replaceNA(c(1, 2, NA, 3, NA), 0))))
})

test_that("compareVersionsMax", {
  expect_true(pacs::compareVersionsMax(c("1.1.1", "1.0.0", "3.3.3")) == "3.3.3")
})

test_that("compareVersionsMin", {
  expect_true(pacs::compareVersionsMin(c("1.1.1", "1.0.0", "3.3.3")) == "1.0.0")
})

test_that("pacs::pac_true_size", {
  stats_size <- pacs::pac_true_size("stats")/10**6
  expect_true(stats_size > 5 && stats_size < 40)
})

test_that("pacs::pac_deps", {
  stats_deps <- pacs::pac_deps("stats")
  expect_true(ncol(stats_deps) == 3)
  expect_true(length(stats_deps$Package) == 3)
  stats_deps_attr <- attributes(stats_deps)
  expect_true(stats_deps_attr$Package == "stats")
  expect_true(stats_deps_attr$class == "data.frame")
  stats_deps2 <- pacs::pac_deps("stats", attr = FALSE)
  expect_true(ncol(stats_deps2) == 3)
  expect_true(length(stats_deps2$Package) == 4)
  shiny_deps_version <- pacs::pac_deps("shiny", version = "1.6.0")
  expect_true(ncol(shiny_deps_version) == 3)

})

test_that("pacs::pacs_deps", {
  expect_true(nrow(pacs_deps(c("stats", "base", "methods"))) == 6 )
})

test_that("pacs::pac_compare_versions", {
  vers <- pacs::pac_compare_versions("shiny", "1.4.0-2", "1.5.0")
  expect_true(nrow(vers) == 5)
})

test_that("dir_size", {
  current_dir <- dir_size(".")
  expect_true(current_dir > 0)
})
