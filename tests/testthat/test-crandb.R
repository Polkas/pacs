test_that("crandb", {
  expect_identical(get_lifedurations_vec(c("dplyr"), c("1.0.7", "1.6.0"), source = "crandb"), NA)
  expect_identical(get_lifedurations_vec(c("dplyr", "shiny", "data.table"), c("1.0.7", "1.6.0"), source = "crandb"), c(NA, NA, NA))
  expect_identical(get_lifedurations_vec(c("dplyr", "shiny"), c("1.0.7", "1.6.0"), source = "crandb"), c(235, 240))
  expect_identical(get_lifedurations_vec(c("dplyr", "shiny"), c("1.0.7", "1.6.0"), source = "cran"), c(234, 240))
  expect_identical(get_crandb_lifedurations(c("dplyr", "shiny"), c("1.0.7", "1.6.0")),
                   structure(list(Package = c("dplyr", "shiny"), lifeduration = c(235, 240)), class = "data.frame", row.names = c("dplyr", "shiny")))
  expect_identical(get_crandb_lifedurations(c("dplyr", "shiny", "3"), c("1.0.7", "1.6.0")),
                   c(NA, NA, NA))
})

