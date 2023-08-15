test_that("pacs::pac_timemachine", {
  expect_identical(suppressWarnings(pac_timemachine("WRONG")), NA)
  expect_error(pac_timemachine("dplyr", version = 2))
})

test_that("pacs::pac_timemachine online", {
  skip_if_offline()
  expect_true(pac_timemachine("memoise", at = as.Date("2017-02-02"))$Version == "1.0.0")
  expect_true(pac_timemachine("memoise", at = as.Date("2017-02-02"), source = "cran")$Version == "1.0.0")
  expect_true(nrow(pac_timemachine("memoise", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))) == 2)
  expect_true(nrow(pac_timemachine("memoise", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"), source = "cran")) == 2)
  expect_identical(nrow(pac_timemachine("dplyr", version = "999.1.1.1")), 0L)
  expect_true(nrow(pac_timemachine("dplyr")) >= 0)
  expect_true(nrow(pac_timemachine("memoise", at = as.Date("2100-01-01"))) == 1)
  expect_true(nrow(pac_timemachine("memoise", from = as.Date("2100-01-01"), to = as.Date("2200-01-01"))) == 1)
  expect_true(isNA(pac_archived("WRONG")))
  expect_true(is.data.frame(pac_archived("dplyr")))
})

test_that("pacs::pac_timemachine offline", {
  pac_timemachine_offline <- pac_timemachine
  mockery::stub(pac_timemachine_offline, "is_online", FALSE)
  expect_message(pac_timemachine_offline("dplyr"), "No internet connection detected")
  expect_true(isNA(suppressMessages(pac_timemachine_offline("dplyr"))))
})
