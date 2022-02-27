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

test_that("pacs::lock_validate", {
  expect_error(suppressWarnings(lock_validate(path = "files/wrong.lock")))
  expect_error(lock_validate(path = "files/renv_test.lock", checkred = "STH"))
  expect_error(lock_validate(path = "files/renv_test.lock", checkred = list(scope = "ERROR"), lifeduration = "None"))
})

test_that("pacs::lock_validate", {
  skip_if_offline()
  expect_true(is.data.frame(lock_validate(path = "files/renv_test.lock")))
  expect_true(is.data.frame(lock_validate(path = "files/renv_test.lock", checkred = list(scope = "ERROR"), lifeduration = TRUE)))
})

test_that("lib_validate lifedurations", {
  skip_if(nrow(installed_packages(lib.loc = .libPaths())) > getOption("pacs.crandb_limit", 100))
  skip_if_offline()
  lib_res <- lib_validate(lifeduration = TRUE)
  expect_true(inherits(lib_res, "data.frame"))
  expect_true(all(is.na(lib_res$lifeduration) | (lib_res$lifeduration >= 0)))
})

test_that("pacs::pac_validate", {
  skip_if_offline()
  skip_if(isNA(checked_packages()))
  expect_true(nrow(pac_validate("stats")) == 0)
  pac_valid_full <- pac_validate("memoise",
    lifeduration = TRUE,
    checkred = list(scope = c("ERROR", "FAIL"), flavors = NULL)
  )
  expect_true(inherits(pac_valid_full, "data.frame"))
})

test_that("offline validate", {
  pac_validate_offline <- pac_validate
  mockery::stub(pac_validate_offline, "is_online", FALSE)
  expect_warning(pac_validate_offline("memoise"), "There is no Internet connection")

  lib_validate_offline <- lib_validate
  mockery::stub(lib_validate_offline, "is_online", FALSE)
  expect_warning(lib_validate_offline(), "There is no Internet connection")

  lock_validate_offline <- lock_validate
  mockery::stub(lock_validate_offline, "is_online", FALSE)
  expect_warning(lock_validate_offline("files/renv_test.lock"), "There is no Internet connection")
})

test_that("lock_validate skip crandb if the limit is exceeded", {
  expect_message(withr::with_options(list(pacs.crandb_limit = 1), {
    lock_validate("tests/testthat/files/renv_test.lock")
  }), "There is more than")
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
  expect_true(isNA(pac_lifeduration("edgeR")) || inherits(pac_lifeduration("edgeR"), "difftime"))
  expect_true(isNA(pac_lifeduration("edgeR")) || inherits(pac_lifeduration("edgeR"), "difftime"))
})

test_that("pacs::pac_health", {
  expect_true(is.logical(pac_health("stats")))
  expect_true(is.na(pac_health("WRONG")))
  expect_true(is.na(pac_health("dplyr", "0.0.0.1")))
})

test_that("pacs::pac_health online", {
  skip_if_offline()
  skip_if(isNA(checked_packages()))
  expect_true(is.logical(pac_health("dplyr")))
  expect_true(is.logical(pac_health("dplyr", scope = c("ERROR"))))
  expect_true(isFALSE(pac_health("dplyr", version = "0.8.0")) ||
    isNA(pac_health("dplyr", version = "0.8.0")))
  expect_true(isFALSE(pac_health("dplyr", at = as.Date("2019-02-14"))) ||
    isNA(pac_health("dplyr", at = as.Date("2019-02-14"))))
})

test_that("pacs::pac_checkred", {
  expect_error(pac_checkred("dplyr", scope = ""))
})

test_that("pacs::pac_checkred online", {
  skip_if_offline()
  skip_if(isNA(checked_packages()))
  expect_true(is.logical(pac_checkred("dplyr")))
  expect_true(is.na(pac_checkred("WRONG")))
  expect_true(isNA(loc <- pac_checkred("dplyr",
    scope = c("ERROR", "FAIL", "WARN"),
    flavors = c(
      "r-devel-linux-x86_64-debian-clang",
      "r-devel-linux-x86_64-debian-gcc"
    )
  )) || is.logical(loc))
})
