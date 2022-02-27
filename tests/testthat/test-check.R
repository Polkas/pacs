test_that("pac_islast", {
  skip_if_offline()
  expect_identical(pac_islast("WRONG"), FALSE)
  expect_true(is.logical(pac_islast("dplyr")))
  expect_true(is.logical(pac_islast("ThreeWiseMonkeys")))
})

test_that("pac_isin", {
  skip_if_offline()
  expect_identical(pac_isin("WRONG"), FALSE)
  expect_true(is.logical(pac_isin("dplyr")))
  expect_true(is.logical(pac_isin("ThreeWiseMonkeys")))
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

test_that("pacs::pac_checkred offline", {
  pac_checkred_offline <- pac_checkred
  mockery::stub(pac_checkred_offline, "is_online", FALSE)
  expect_identical(pac_checkred_offline("dplyr"), NA)
})
