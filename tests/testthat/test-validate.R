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
  expect_warning(lock_validate_offline("files/renv_test.lock"), "There is no Internet connection.")
})

test_that("lock_validate skip crandb if the limit is exceeded", {
  skip_if_offline()
  expect_warning(withr::with_options(list(pacs.crandb_limit = 1), {
    lock_validate("files/renv_test.lock")
  }), "There is more packages than crandb limit of 1")
  expect_message(suppressWarnings(withr::with_options(list(pacs.crandb_limit = 1), {
    lock_validate("files/renv_test.lock", lifeduration = TRUE)
  })), "Please wait, Packages life")
  expect_warning(suppressMessages(withr::with_options(list(pacs.crandb_limit = 1), {
    lock_validate("files/renv_test.lock", lifeduration = TRUE)
  })), "There is more packages than crandb limit of 1.")
})

test_that("lock_validate lifedurations to many packages for crandb", {
  skip_if_offline()
  expect_message(suppressWarnings(withr::with_options(list(pacs.crandb_limit = 1), {
    lock_validate("files/renv_test.lock", lifeduration = TRUE)
  })), "Please wait, Packages life durations")
})

test_that("pacs::lib_validate offline", {
  lib_validate_offline <- lib_validate
  mockery::stub(lib_validate_offline, "is_online", FALSE)
  expect_true(ncol(suppressWarnings(lib_validate_offline())) == 4)
})

test_that("pacs::pac_validate offline", {
  pac_validate_offline <- pac_validate
  mockery::stub(pac_validate_offline, "is_online", FALSE)
  expect_true(ncol(suppressWarnings(pac_validate_offline("memoise"))) == 5)
})

test_that("pacs::lock_validate offline", {
  lock_validate_offline <- lock_validate
  mockery::stub(lock_validate_offline, "is_online", FALSE)
  expect_true(ncol(suppressWarnings(lock_validate_offline("files/renv_test.lock"))) == 2)
})
