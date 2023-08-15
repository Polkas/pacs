test_that("pacs::pac_lifeduration", {
  skip_if_offline()
  expect_message(pac_lifeduration("WRONGPACKAGE"), "WRONGPACKAGE package is not in provided repositories")
  expect_true(is.na(suppressMessages(pac_lifeduration("WRONGPACKAGE"))))
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
  skip_if_offline()
  expect_true(is.logical(pac_health("stats")))
  expect_message(pac_health("WRONG"), "WRONG package is not in provided repositories")
  expect_true(is.na(suppressMessages(pac_health("WRONG"))))
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
