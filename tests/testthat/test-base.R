test_that("replaceNA", {
  expect_true(all(!is.na(replaceNA(c(1, 2, NA, 3, NA), 0))))
})

test_that("compareVersionsMax", {
  expect_true(pacs::compareVersionsMax(c("1.1.1", "1.0.0", "3.3.3")) == "3.3.3")
  expect_true(pacs::compareVersionsMax("3.3.3") == "3.3.3")

})

test_that("compareVersionsMin", {
  expect_true(pacs::compareVersionsMin(c("1.1.1", "1.0.0", "3.3.3")) == "1.0.0")
  expect_true(pacs::compareVersionsMin("1.1.1") == "1.1.1")
})

test_that("pacs::pac_deps", {
  stats_deps <- pacs::pac_deps("stats", base = TRUE)
  stats_deps_tools <- tools::package_dependencies("stats",
                                                  recursive = TRUE,
                                                  db = installed.packages())[[1]]
  expect_true(ncol(stats_deps) > 0)
  expect_true(length(stats_deps$Package) > 0 && length(stats_deps_tools) > 0)
  stats_deps_attr <- attributes(stats_deps)
  expect_true(stats_deps_attr$Package == "stats")
  expect_true(stats_deps_attr$class == "data.frame")
  stats_deps2 <- pacs::pac_deps("stats", attr = FALSE, base = TRUE)
  expect_true(ncol(stats_deps2) > 0)
  expect_true(length(stats_deps2$Package) > 0)
})

test_that("pacs::pacs_deps", {
  expect_true(nrow(pacs_deps(c("stats", "tools", "methods"), base = TRUE)) > 0)
  expect_true(nrow(pacs_deps(c("stats", "tools", "methods"), base = TRUE, description_v = T)) > 0)

})

test_that("dir_size", {
  current_dir <- dir_size(".")
  expect_true(current_dir > 0)
})

test_that("pacs::pac_size", {
  expect_true(pacs::pac_size("stats") > 0)
})

test_that("pacs::pacs_size", {
  expect_true(length(pacs::pacs_size(c("stats", "methods"))) > 0)
  expect_true(sum(pacs::pacs_size(c("stats", "methods"))) > 0)

})

test_that("pacs::pac_true_size", {
  stats_size <- pacs::pac_true_size("stats", base = TRUE) / 10**6
  expect_true(stats_size > 1)
  stats_size2 <- pacs::pac_true_size("stats", base = TRUE, exclude_joint = 1L)
  expect_equal(stats_size2, 0)
})

test_that("pacs::lib_validate", {
  expect_error(lib_validate(lib.loc = "wrong"))
  expect_true(inherits(lib_validate(), "data.frame"))
})

test_that("pacs::pacs_size", {
  expect_true(length(pacs_size()) > 1)
})

test_that("pac_validate", {
  expect_true(nrow(pac_validate("stats")) == 0)
})

test_that("pac_validate", {
  expect_true(nrow(pacs_validate(c("stats", "graphics"))) == 0)
})


test_that("pacs_base", {
  expect_true(all(c("stats", "methods", "base", "utils", "graphics") %in% pacs_base()))
  expect_true( length(pacs_base()) >= length(pacs_base(startup = TRUE)))
})

if (is_online()) {
test_that("pac_timemachine", {
  expect_true(pac_timemachine("dplyr", at = as.Date("2017-02-02"))$Version == "0.5.0")
  expect_true(nrow(pac_timemachine("dplyr", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))) == 6)
})

test_that("pacs_timemachine", {
  expect_identical(vapply(pacs_timemachine(c("dplyr", "shiny"), from = as.Date("2018-06-30"), to = as.Date("2019-01-01")),
                          function(x) nrow(x),
                          numeric(1)),
                   c(dplyr = 3, shiny = 2))
  expect_identical(vapply(pacs_timemachine(c("dplyr", "shiny"), at = Sys.Date()),
                          function(x) nrow(x),
                          numeric(1)),
                   c(dplyr = 1, shiny = 1))
})

test_that("pac_health", {
  expect_true(isFALSE(pac_health("dplyr", version = "0.8.0")))
})

test_that("pacs_health", {
  expect_equal(pacs_health(c("dplyr", "devtools"),
                           versions = c("0.8.0", "2.4.0")),
               list(structure(FALSE, class = "sure"),
                    structure(TRUE, class = "sure")))
})

}
