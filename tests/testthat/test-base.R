test_that("replaceNA", {
  expect_true(all(!is.na(replaceNA(c(1, 2, NA, 3, NA), 0))))
})

test_that("compareVersionsMax", {
  expect_true(pacs::compareVersionsMax(c("1.1.1", "1.0.0", "3.3.3")) == "3.3.3")
})

test_that("compareVersionsMin", {
  expect_true(pacs::compareVersionsMin(c("1.1.1", "1.0.0", "3.3.3")) == "1.0.0")
})

test_that("pacs::pac_deps", {
  stats_deps <- pacs::pac_deps("stats")
  stats_deps_tools <- tools::package_dependencies("stats",
                                                  recursive = TRUE,
                                                  db = installed.packages())[[1]]
  expect_true(ncol(stats_deps) == 3)
  expect_true(length(stats_deps$Package) == 3 && length(stats_deps_tools) == 3)
  stats_deps_attr <- attributes(stats_deps)
  expect_true(stats_deps_attr$Package == "stats")
  expect_true(stats_deps_attr$class == "data.frame")
  stats_deps2 <- pacs::pac_deps("stats", attr = FALSE)
  expect_true(ncol(stats_deps2) == 3)
  expect_true(length(stats_deps2$Package) == 4)
})

test_that("pacs::pacs_deps", {
  expect_true(nrow(pacs_deps(c("stats", "tools", "methods"))) == 6 )
})

test_that("dir_size", {
  current_dir <- dir_size(".")
  expect_true(current_dir > 0)
})

test_that("pacs::pac_size", {
  expect_true(pacs::pac_size("stats") > 0)
})

test_that("pacs::pacs_size", {
  expect_true(length(pacs::pacs_size(c("stats", "methods"))) == 2)
  expect_true(sum(pacs::pacs_size(c("stats", "methods"))) > 0)

})

test_that("pacs::pac_true_size", {
  stats_size <- pacs::pac_true_size("stats")/10**6
  expect_true(stats_size > 5 && stats_size < 40)
})
