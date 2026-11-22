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

test_that("extract_deps", {
  input <- c(
    "ggplot2 (>= 3.1.0)", "shiny (>= 1.3.1)", "", "R (>= 3.1.0)",
    "Biobase", "glmnet", "methods", "stats", "NA"
  )
  expect_identical(
    extract_deps(input),
    list(
      packages = list(
        "ggplot2", "shiny", "", "R", "Biobase",
        "glmnet", "methods", "stats", "NA"
      ),
      versions = list(
        "3.1.0", "1.3.1", NA_character_, "3.1.0",
        NA_character_, NA_character_, NA_character_,
        NA_character_, NA_character_
      )
    )
  )
})

test_that("package load preserves user options and supplies missing defaults", {
  withr::with_options(list(pacs.crandb_limit = 77, pacs.crandb_ntry = 8, pacs.crandb_nsleep = 4), {
    pacs:::.onLoad(NULL, NULL)
    expect_identical(getOption("pacs.crandb_limit"), 77)
    expect_identical(getOption("pacs.crandb_ntry"), 8)
    expect_identical(getOption("pacs.crandb_nsleep"), 4)
  })
  withr::with_options(list(pacs.crandb_limit = NULL, pacs.crandb_ntry = NULL, pacs.crandb_nsleep = NULL), {
    pacs:::.onLoad(NULL, NULL)
    expect_identical(getOption("pacs.crandb_limit"), 500)
    expect_identical(getOption("pacs.crandb_ntry"), 3)
    expect_identical(getOption("pacs.crandb_nsleep"), 0.1)
  })
})
