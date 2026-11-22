test_that("repository membership and latest versions include incompatible packages", {
  packages <- matrix(
    c("example", "2.0.0", "R (>= 4.1.0)"),
    nrow = 1,
    dimnames = list("example", c("Package", "Version", "Depends"))
  )

  is_in <- is_isin_raw
  mockery::stub(is_in, "available_packages_all", packages)
  expect_true(is_in("example", "https://example.test"))

  latest <- last_version_raw
  mockery::stub(latest, "available_packages_all", packages)
  expect_identical(latest("example", "https://example.test"), "2.0.0")
  expect_identical(latest("missing", "https://example.test"), NA_character_)
})

test_that("latest-version lookup disables only R and platform filters", {
  packages <- matrix(
    c("example", "2.0.0", "R (>= 4.1.0)"),
    nrow = 1,
    dimnames = list("example", c("Package", "Version", "Depends"))
  )
  get_packages <- available_packages_all
  mockery::stub(get_packages, "available_packages_raw", function(repos, filters) {
    expect_identical(filters, "duplicates")
    packages
  })

  expect_identical(
    get_packages("https://example.test")["example", "Version"],
    "2.0.0"
  )
})

test_that("dependency resolution reports when a repository package is incompatible", {
  dependencies <- pac_deps
  mockery::stub(dependencies, "is_online", TRUE)
  mockery::stub(dependencies, "pac_isin", TRUE)
  mockery::stub(
    dependencies,
    "available_packages",
    matrix(
      c("other", "1.0.0"),
      nrow = 1,
      dimnames = list("other", c("Package", "Version"))
    )
  )

  expect_message(
    expect_identical(
      dependencies("example", local = FALSE, repos = "https://example.test"),
      NA
    ),
    "not available for R"
  )
})

test_that("CRAN file lookup stops when the package has no repository version", {
  read_file <- read_cran_file
  mockery::stub(read_file, "is_online", TRUE)
  mockery::stub(read_file, "pac_last", NA_character_)

  expect_identical(
    read_file("missing", "1.0.0", "DESCRIPTION"),
    NA
  )
})
