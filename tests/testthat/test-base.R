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
    db = installed.packages()
  )[[1]]
  expect_true(ncol(stats_deps) > 0)
  expect_true(length(stats_deps$Package) > 0 && length(stats_deps_tools) > 0)
  stats_deps_attr <- attributes(stats_deps)
  expect_true(stats_deps_attr$Package == "stats")
  expect_true(stats_deps_attr$class == "data.frame")
  stats_deps2 <- pacs::pac_deps("stats", attr = FALSE, base = TRUE)
  expect_true(ncol(stats_deps2) > 0)
  expect_true(length(stats_deps2$Package) > 0)

  expect_true(ncol(pacs::pac_deps("memoise", description_v = TRUE, recursive = FALSE)) == 2)
})

test_that("dir_size", {
  current_dir <- dir_size(".")
  expect_true(current_dir > 0)
})

test_that("pacs::pac_size", {
  expect_true(pacs::pac_size("stats") > 0)
})

test_that("pacs::pac_true_size", {
  stats_size <- pacs::pac_true_size("stats") / 10**6
  expect_true(stats_size > 1)
  stats_size2 <- pacs::pac_true_size("stats", exclude_joint = 1L)
  expect_equal(stats_size2, pac_size("stats"))
})

test_that("pacs_base", {
  expect_true(all(c("stats", "methods", "base", "utils", "graphics") %in% pacs_base()))
  expect_true(length(pacs_base()) >= length(pacs_base(startup = TRUE)))
})

if (is_online()) {
  test_that("pacs::pac_compare_versions", {
    expect_true(nrow(pac_compare_versions("memoise", "0.2.1", "2.0.0")) == 3)
  })

  test_that("pacs::pac_deps_timemachine", {
    expect_true(length(pac_deps_timemachine("memoise", "0.2.1")) == 1)
  })

  test_that("pacs::lib_validate", {
    expect_error(lib_validate(lib.loc = "wrong"))
    lib_res <- lib_validate()
    expect_true(inherits(lib_res, "data.frame"))
  })

  test_that("pac_validate", {
    expect_true(nrow(pac_validate("stats")) == 0)
  })

  test_that("pac_timemachine", {
    expect_true(pac_timemachine("memoise", at = as.Date("2017-02-02"))$Version == "1.0.0")
    expect_true(nrow(pac_timemachine("memoise", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))) == 2)
  })

  test_that("pac_lifeduration", {
    a <- pac_lifeduration("dplyr", version = "0.8.0")
    b <- pac_lifeduration("dplyr", at = as.Date("2019-02-14"))
    expect_true(a == 1)
    expect_identical(a, b)
    expect_true(is.na(pac_lifeduration("WRONGPACKAGE")))
    expect_error(pac_lifeduration("dplyr", version = 1))
  })

  test_that("pac_health", {
    expect_true(isFALSE(pac_health("dplyr", version = "0.8.0")))
    expect_true(is.logical(pac_health("dplyr")))
  })

  test_that("pac_description", {
    expect_true(length(pac_description("dplyr", version = "0.8.0")) == 23)
    expect_true(utils::compareVersion(pac_description("memoise", local = TRUE)$Version,
                                      pac_description("memoise", local = FALSE)$Version) %in% c(0, 1))
  })

  test_that("pac_last", {
    expect_identical(
    unname(available.packages(repos = "https://cran.rstudio.com/", filters = list(
      function (db) db[db[,"Package"] == "dplyr", ]
    ))["Version"]),
    pac_last("dplyr", repos = "https://cran.rstudio.com/")
    )
  })

}
