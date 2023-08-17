test_that("crandb", {
  skip_if_offline()
  skip_on_cran()
  expect_identical(pacs_lifeduration(c("dplyr"), c("1.0.7", "1.6.0")), NA)
  expect_identical(pacs_lifeduration(c("dplyr", "shiny", "data.table"), c("1.0.7", "1.6.0")), c(NA, NA, NA))
  res <- pacs_lifeduration(c("dplyr", "shiny"), c("1.0.7", "1.6.0"))
  expect_true(anyNA(res) || identical(res$lifeduration, c(235, 240)))
  res <- pacs_lifeduration(c("dplyr", "shiny"), c("1.0.7", "1.6.0"), source = "loop_crandb")
  expect_true(anyNA(res) || identical(res$lifeduration, c(234, 240)))
  res <- pacs_lifeduration(c("dplyr", "shiny"), c("1.0.7", "1.6.0"))
  expect_true(
    anyNA(res) ||
      identical(
        res,
        structure(list(Package = c("dplyr", "shiny"), lifeduration = c(235, 240)), class = "data.frame", row.names = c(NA, -2L))
      )
  )
  expect_identical(
    pacs_lifeduration(c("dplyr", "shiny", "3"), c("1.0.7", "1.6.0")),
    c(NA, NA, NA)
  )
})
