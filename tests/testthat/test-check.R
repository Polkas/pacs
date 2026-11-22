test_that("pac_islast", {
  skip_if_offline()
  expect_identical(pac_islast("WRONG"), FALSE)
  expect_true(is.logical(pac_islast("memoise")))
  expect_true(is.logical(pac_islast("dplyr")))
  expect_true(is.logical(pac_islast("dplyr", "1.0.0")))
  expect_true(is.logical(pac_islast("ThreeWiseMonkeys")))
})

test_that("pac_isin", {
  skip_if_offline()
  expect_identical(pac_isin("WRONG"), FALSE)
  expect_true(is.logical(pac_isin("dplyr")))
  expect_true(is.logical(pac_isin("ThreeWiseMonkeys")))
})

test_that("pacs::pac_checkred online", {
  skip_if_offline()
  skip_on_cran()
  expect_error(pac_checkred("dplyr", scope = ""), "")
  skip_if(isNA(checked_packages()))
  expect_true(is.logical(pac_checkred("dplyr")) || isNA(pac_checkred("dplyr")))
  expect_message(pac_checkred("WRONG"), "WRONG package is not on CRAN")
  expect_true(isNA(suppressMessages(pac_checkred("WRONG"))))
  expect_true(isNA(loc <- pac_checkred("dplyr",
    scope = c("ERROR", "FAIL", "WARN"),
    flavors = c(
      "r-devel-linux-x86_64-debian-clang",
      "r-devel-linux-x86_64-debian-gcc"
    )
  )) || is.logical(loc))
})

test_that("pacs::pac_checkred offline", {
  pac_checkred_offline <- pac_checkred
  mockery::stub(pac_checkred_offline, "is_online", FALSE)
  expect_message(pac_checkred_offline("dplyr"), "No internet connection detected")
  expect_identical(suppressMessages(pac_checkred_offline("dplyr")), NA)
})

test_that("pac_checkred handles starred and unavailable statuses", {
  check <- pac_checkred
  mockery::stub(check, "is_online", TRUE)
  mockery::stub(check, "pac_isin", TRUE)
  mockery::stub(check, "cran_flavors", data.frame(Flavor = c("ok", "error", "missing")))
  mockery::stub(check, "pac_checkpage", data.frame(
    Flavor = c("ok", "error"),
    Status = c("OK", "ERROR*"),
    stringsAsFactors = FALSE
  ))

  expect_true(check("example"))
  expect_true(check("example", flavors = "error"))
  expect_false(check("example", flavors = "ok"))
  expect_false(check("example", scope = character()))
  expect_false(check("example", scope = NULL))
  expect_message(expect_identical(check("example", flavors = "missing"), NA), "No package check statuses")

  mockery::stub(check, "pac_checkpage", NA)
  expect_message(expect_identical(check("example"), NA), "Failed to retrieve package check statuses")
  mockery::stub(check, "pac_checkpage", data.frame(Flavor = "ok", Status = NA_character_))
  expect_message(expect_identical(check("example"), NA), "statuses are unavailable")
  mockery::stub(check, "pac_checkpage", data.frame(Flavor = "ok", Status = ""))
  expect_message(expect_identical(check("example"), NA), "statuses are unavailable")
  mockery::stub(check, "pac_checkpage", data.frame(Flavor = "ok", Status = "UNKNOWN"))
  expect_message(expect_identical(check("example"), NA), "statuses are unavailable")
  mockery::stub(check, "pac_checkpage", data.frame(Flavor = c("ok", "error"), Status = c("", "ERROR*")))
  expect_true(check("example"))
  mockery::stub(check, "cran_flavors", NA)
  expect_message(expect_identical(check("example", flavors = "ok"), NA), "Failed to retrieve CRAN check flavors")
})
