test_that("pacs::lib_validate", {
  expect_error(lib_validate(lib.loc = "wrong"))
  expect_error(lib_validate(checkred = TRUE))
  expect_error(lib_validate(checkred = "ERROR"))
})

test_that("pacs::lib_validate all needed packages", {
  skip_on_cran()
  skip_if_offline()
  expect_identical(
    sort(unique(rownames(installed_packages(lib.loc = .libPaths())))),
    sort(unique(setdiff(c(lib_validate()[!is.na(lib_validate()$Version.have), ]$Package, pacs_base()), "R")))
  )
})

test_that("pacs::lib_validate online", {
  skip_if_offline()
  lib_res <- lib_validate()
  expect_true(inherits(lib_res, "data.frame"))
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
  skip_if_offline()
  expect_true(nrow(pac_validate("stats")) == 0)
  pac_valid_full <- pac_validate("memoise",
    lifeduration = TRUE,
    checkred = list(scope = c("ERROR", "FAIL"), flavors = NULL)
  )
  expect_true(inherits(pac_valid_full, "data.frame"))
})

test_that("pacs::pac_lifeduration", {
  skip_if_offline()
  expect_true(is.na(pac_lifeduration("WRONGPACKAGE")))
  expect_error(pac_lifeduration("dplyr", version = 1))
  expect_true(pac_lifeduration("memoise") > 0)
})

test_that("pacs::pac_lifeduration online", {
  skip_if_offline()
  a <- pac_lifeduration("dplyr", version = "0.8.0")
  b <- pac_lifeduration("dplyr", at = as.Date("2019-02-14"))
  expect_true(a == 1)
  expect_identical(a, b)
  expect_true(is.na(pac_lifeduration("edgeR")) || inherits(pac_lifeduration("edgeR"), "difftimes"))
})

test_that("pacs::pac_health", {
  skip_if_offline()
  expect_true(is.logical(pac_health("stats")))
  expect_true(is.na(pac_health("WRONG")))
  expect_true(is.na(pac_health("dplyr", "0.0.0.1")))
})

test_that("pacs::pac_health online", {
  skip_if_offline()
  expect_true(is.logical(pac_health("dplyr")))
  expect_true(isFALSE(pac_health("dplyr", version = "0.8.0")))
  expect_true(isFALSE(pac_health("dplyr", at = as.Date("2019-02-14"))))
})

test_that("pacs::pac_checkred", {
  expect_error(pac_checkred("dplyr", scope = ""))
})

test_that("pacs::pac_checkred online", {
  skip_if_offline()
  expect_true(is.logical(pac_checkred("dplyr")))
  expect_true(is.na(pac_checkred("WRONG")))
})
