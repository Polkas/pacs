test_that("replaceNA", {
  expect_true(all(!is.na(replaceNA(c(1, 2, NA, 3, NA), 0))))
})

test_that("isNA", {
  expect_true(isNA(NA))
  expect_false(isNA(c(NA, NA)))
  expect_false(isNA(2))
  expect_false(isNA("a"))
  expect_false(isNA(airquality))
})

test_that("pacs::compareVersionsMax", {
  expect_true(pacs::compareVersionsMax(c("1.1.1", "1.0.0", "3.3.3")) == "3.3.3")
  expect_true(pacs::compareVersionsMax("3.3.3") == "3.3.3")
})

test_that("pacs::compareVersionsMin", {
  expect_true(pacs::compareVersionsMin(c("1.1.1", "1.0.0", "3.3.3")) == "1.0.0")
  expect_true(pacs::compareVersionsMin("1.1.1") == "1.1.1")
})

test_that("expand_dependency", {
  expect_identical(expand_dependency("strong"), c("Depends", "Imports", "LinkingTo"))
  expect_identical(expand_dependency("all"), c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
  expect_identical(expand_dependency("most"), c("Depends", "Imports", "LinkingTo", "Suggests"))
  expect_identical(expand_dependency(c("Depends", "Imports", "LinkingTo")), c("Depends", "Imports", "LinkingTo"))
  expect_error(expand_dependency("WRONG"))
})

test_that("pac_islast", {
  skip_if_offline()
  expect_identical(pac_islast("WRONG"), FALSE)
  expect_true(is.logical(pac_islast("memoise")))
})

test_that("pac_checkpage", {
  expect_true(isNA(pac_checkpage("WRONG")))
})

test_that("biocran_repos", {
  expect_identical(biocran_repos(NA), c(CRAN = "https://cran.rstudio.com/"))
})
