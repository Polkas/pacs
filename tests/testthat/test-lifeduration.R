test_that("pacs::pac_lifeduration", {
  skip_if_offline()
  expect_message(pac_lifeduration("WRONGPACKAGE"), "WRONGPACKAGE package is not in provided repositories")
  expect_true(isNA(suppressMessages(pac_lifeduration("WRONGPACKAGE"))))
  expect_error(pac_lifeduration("dplyr", version = 1))
  expect_true(pac_lifeduration("memoise") > 0)
})

test_that("pacs::pac_lifeduration online", {
  skip_if_offline()
  res <- pac_lifeduration("dplyr", version = "0.8.0")
  expect_true(isNA(res) || (res == 1))
  res <- pac_lifeduration("dplyr", at = as.Date("2019-02-14"))
  expect_true(isNA(res) || (res == 1))
  res <- pac_lifeduration("edgeR")
  expect_true(isNA(res) || inherits(res, "difftime"))
  res <- pac_lifeduration("edgeR")
  expect_true(isNA(res) || inherits(res, "difftime"))
})

test_that("pacs::pac_health", {
  skip_if_offline()
  expect_true(is.logical(pac_health("stats")))
  expect_message(pac_health("WRONG"), "WRONG package is not in provided repositories")
  expect_true(isNA(suppressMessages(pac_health("WRONG"))))
  expect_true(isNA(pac_health("dplyr", "0.0.0.1")))
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
  expect_true(isNA(suppressMessages(pac_lifeduration("WRONGPACKAGE"))))
  expect_error(pac_lifeduration("dplyr", version = 1))
  expect_true(pac_lifeduration("memoise") > 0)
})

test_that("pac_health checks the effective newest version", {
  health <- pac_health
  mockery::stub(health, "is_online", TRUE)
  mockery::stub(health, "pacs_base", character())
  mockery::stub(health, "pac_lifeduration", 30)
  mockery::stub(health, "pac_last", "1.0")
  mockery::stub(health, "pac_checkred", TRUE)
  mockery::stub(health, "pac_timemachine", function(pac, at, source) {
    stopifnot(source == "crandb")
    data.frame(Version = "1.0", stringsAsFactors = FALSE)
  })

  expect_false(health("example"))
  expect_false(health("example", version = "1.0"))
  expect_false(health("example", at = as.Date("2020-01-01"), source = "crandb"))
  expect_true(health("example", version = "0.9"))
  expect_true(health("example", scope = character()))

  mockery::stub(health, "pac_checkred", NA)
  expect_identical(health("example"), NA)
  mockery::stub(health, "pac_lifeduration", 1)
  expect_false(health("example"))

  mockery::stub(health, "pac_lifeduration", 30)
  mockery::stub(health, "pac_last", NA)
  expect_message(expect_identical(health("example"), NA), "newest package version")
})

test_that("pac_lifeduration uses the requested historical source", {
  lifetime <- pac_lifeduration
  mockery::stub(lifetime, "is_online", TRUE)
  mockery::stub(lifetime, "is_isin", TRUE)
  mockery::stub(lifetime, "pac_last", "1.0")
  mockery::stub(lifetime, "installed_packages", matrix(character(), nrow = 0, ncol = 1))
  mockery::stub(lifetime, "pac_timemachine", function(pac, at = NULL, version = NULL, source) {
    stopifnot(source == "crandb")
    if (!is.null(at)) {
      data.frame(Version = "0.9", stringsAsFactors = FALSE)
    } else {
      data.frame(LifeDuration = as.difftime(10, units = "days"))
    }
  })

  expect_equal(as.numeric(lifetime("example", at = as.Date("2020-01-01"), source = "crandb", repos = "https://example.test")), 10)
  mockery::stub(lifetime, "pac_timemachine", data.frame(Version = character()))
  expect_identical(lifetime("example", at = as.Date("1900-01-01"), repos = "https://example.test"), NA)
})
