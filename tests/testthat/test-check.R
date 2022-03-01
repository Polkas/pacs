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
