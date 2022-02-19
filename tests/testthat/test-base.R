test_that("pacs::pacs_base", {
  expect_true(all(c("stats", "methods", "base", "utils", "graphics") %in% pacs_base()))
  expect_true(length(pacs_base()) >= length(pacs_base(startup = TRUE)))
})
