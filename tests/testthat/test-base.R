test_that("pacs::replaceNA", {
  expect_true(all(!is.na(replaceNA(c(1, 2, NA, 3, NA), 0))))
})

test_that("pacs::compareVersionsMax", {
  expect_true(pacs::compareVersionsMax(c("1.1.1", "1.0.0", "3.3.3")) == "3.3.3")
  expect_true(pacs::compareVersionsMax("3.3.3") == "3.3.3")
})

test_that("pacs::compareVersionsMin", {
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

test_that("pacs::pacs_base", {
  expect_true(all(c("stats", "methods", "base", "utils", "graphics") %in% pacs_base()))
  expect_true(length(pacs_base()) >= length(pacs_base(startup = TRUE)))
})

if (is_online()) {
  test_that("pacs::pac_compare_versions", {
    expect_true(nrow(pac_compare_versions("memoise", "0.2.1", "2.0.0")) == 3)
    expect_true(suppressWarnings(any(duplicated(colnames(pac_compare_versions("memoise", "2.0.0", "2.0.0"))))))
    expect_error(pac_compare_versions("memoise", "2.0.0", "2.4.0"))
    expect_error(pac_compare_versions("memoise", "2.8.0", "2.4.0"))
  })

  test_that("pacs::pac_compare_namesapce", {
    expect_true(length(pac_compare_namespace("memoise", "0.2.1", "2.0.0")) == 10)
    expect_identical(pac_compare_namespace("memoise", "0.2.1", "2.0.0")$exports$added, c("cache_filesystem", "cache_gcs", "cache_memory", "cache_s3",
                                                                               "drop_cache", "has_cache", "timeout"))
    expect_error(pac_compare_namespace("memoise", "2.0.0", "2.4.0"))
    expect_error(pac_compare_namespace("memoise", "2.8.0", "2.4.0"))
  })

  test_that("pacs::pac_deps_timemachine", {
    expect_true(length(pac_deps_timemachine("memoise", "0.2.1")) == 1)
  })

  test_that("pacs::lib_validate", {
    expect_error(lib_validate(lib.loc = "wrong"))
    lib_res <- lib_validate()
    expect_true(inherits(lib_res, "data.frame"))
    expect_error(lib_validate(checkred = TRUE))
    expect_error(lib_validate(checkred = "ERROR"))
    lib_res2 <- lib_validate(checkred = list(scope = c("ERROR", "FAIL")))
    lib_res3 <- lib_validate(checkred = list(scope = c("ERROR", "FAIL", "WARN")))
    lib_res4 <- lib_validate(checkred = list(scope = c("ERROR", "FAIL", "WARN", "NOTE")))
    expect_true(sum(lib_res3$checkred, na.rm = TRUE) >= sum(lib_res2$checkred, na.rm = TRUE))
    expect_true(sum(lib_res4$checkred, na.rm = TRUE) >= sum(lib_res3$checkred, na.rm = TRUE))
    lib_res_s1 <- lib_validate(checkred = list(scope = c("ERROR", "FAIL"), flavors = cran_flavors()$Flavor[1]))
    lib_res_s2 <- lib_validate(checkred = list(scope = c("ERROR", "FAIL"), flavors = cran_flavors()$Flavor[1:4]))
    expect_true(sum(lib_res_s2$checkred, na.rm = TRUE) >= sum(lib_res_s1$checkred, na.rm = TRUE))
  })

  test_that("pacs::pac_validate", {
    expect_true(nrow(pac_validate("stats")) == 0)
  })

  test_that("pacs::pac_timemachine", {
    expect_true(pac_timemachine("memoise", at = as.Date("2017-02-02"))$Version == "1.0.0")
    expect_true(nrow(pac_timemachine("memoise", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))) == 2)
    expect_error(pac_timemachine("WRONG"))
    expect_error(pac_timemachine("dplyr", version = 2))
    expect_identical(nrow(pac_timemachine("dplyr", version = "999.1.1.1")), 0L)
    })

  test_that("pacs::pac_lifeduration", {
    a <- pac_lifeduration("dplyr", version = "0.8.0")
    b <- pac_lifeduration("dplyr", at = as.Date("2019-02-14"))
    expect_true(a == 1)
    expect_identical(a, b)
    expect_true(is.na(pac_lifeduration("WRONGPACKAGE")))
    expect_error(pac_lifeduration("dplyr", version = 1))
    expect_error(pac_lifeduration("dplyr", version = 1))
    expect_true(pac_lifeduration("memoise") > 0)
  })

  test_that("pacs::pac_health", {
    expect_true(isFALSE(pac_health("dplyr", version = "0.8.0")))
    expect_true(is.logical(pac_health("dplyr")))
    expect_true(is.na(pac_health("WRONG")))
  })

  test_that("pacs::pac_description", {
    expect_true(length(pac_description("dplyr", version = "0.8.0")) == 23)
    expect_true(utils::compareVersion(pac_description("memoise", local = TRUE)$Version,
                                      pac_description("memoise", local = FALSE)$Version) %in% c(0, 1))
    expect_identical(pac_description("dplyr", "1.1.1.1"), list())
    expect_identical(pac_description("WRONG"), list())
    expect_identical(suppressWarnings(pac_description("dplyr", "0.0.0.1")), list())
  })

  test_that("pacs::pac_last", {
    expect_identical(
    unname(available.packages(repos = "https://cran.rstudio.com/", filters = list(
      function(db) db[db[,"Package"] == "dplyr", ]
    ))["Version"]),
    pac_last("dplyr", repos = "https://cran.rstudio.com/")
    )
    expect_true(is.na(pac_last("WRONG")))
  })

  test_that("pacs::pac_checkred", {
    expect_true(is.logical(pac_checkred("dplyr")))
    expect_true(is.na(pac_checkred("WRONG")))
    expect_error(pac_checkred("dplyr", scope = ""))
  })

  test_that("pacs::pac_namespace", {
    expect_true(length(pac_namespace("dplyr", version = "0.8.0")) == 10)
    expect_true(length(pac_namespace("shiny")) >= 0)
    expect_true(length(pac_namespace("rlang")) >= 0)
    expect_true(length(pac_namespace("sp")) >= 0)
    expect_true(length(pac_namespace("Rcpp")) >= 0)
    expect_true(length(pac_namespace("float")) >= 0)
    expect_true(length(pac_namespace("xROI")) >= 0)
    expect_true(length(pac_namespace("CNVScope")) >= 0)
    expect_true(length(pac_namespace("GFORCE")) >= 0)
    expect_true(length(pac_namespace("CrossValidate")) >= 0)
    expect_true(length(pac_namespace("classGraph")) >= 0)
    expect_true(length(pac_namespace("mi")) >= 0)
    expect_identical(sort(pac_namespace("memoise", local = TRUE)$exports), sort(base::getNamespaceExports("memoise")))
    expect_identical(pac_namespace("dplyr", "1.1.1.1"), list())
    expect_identical(pac_namespace("WRONG"), list())
    expect_identical(suppressWarnings(pac_namespace("dplyr", "0.0.0.1")), list(imports = list(), exports = character(0), exportPatterns = character(0),
                                                                               importClasses = list(), importMethods = list(), exportClasses = character(0),
                                                                               exportMethods = character(0), exportClassPatterns = character(0),
                                                                               dynlibs = character(0), S3methods = structure(character(0), .Dim = c(0L,
                                                                                                                                                    4L))))
  })

  checked <- pacs::checked_packages()

  test_that("pacs::checked_packages", {
    expect_true(is.data.frame(checked))
    expect_true(nrow(checked) > 0)
    expect_true(all(c("Package", "Version", "Maintainer", "Priority") %in% colnames(checked)))
  })

  flavs <- pacs::cran_flavors()
  test_that("pacs::cran_flavors()", {
    expect_true(all(flavs$Flavor %in% colnames(checked)))
    expect_true(nrow(flavs) > 0)
    expect_true(is.data.frame(flavs))
  })

  test_that("pacs::pac_checkpage", {
    dplyr_checkpage <- pacs::pac_checkpage("dplyr")
    expect_true(all(flavs$Flavor %in% dplyr_checkpage$Flavor))
    expect_true(nrow(dplyr_checkpage) > 0)
    expect_true(is.data.frame(dplyr_checkpage))
  })
}
