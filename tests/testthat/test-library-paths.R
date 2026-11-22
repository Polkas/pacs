make_library_test_package <- function(lib, name, version, imports = NULL, export = "fixture", news = "fixture") {
  path <- file.path(lib, name)
  dir.create(path)
  dir.create(file.path(path, "Meta"))
  description <- c(
    paste("Package:", name),
    paste("Version:", version),
    "Title: Test fixture",
    "Description: An isolated test fixture.",
    "License: MIT",
    "Date/Publication: 2020-01-01 00:00:00 UTC",
    paste("Built: R", getRversion(), "; ; ;", .Platform$OS.type),
    if (!is.null(imports)) paste("Imports:", imports)
  )
  writeLines(description, file.path(path, "DESCRIPTION"))
  writeLines(paste0("export(", export, ")"), file.path(path, "NAMESPACE"))
  writeLines(news, file.path(path, "NEWS"))
  saveRDS(
    list(DESCRIPTION = read.dcf(file.path(path, "DESCRIPTION"))[1, ], Built = list(R = getRversion())),
    file.path(path, "Meta", "package.rds")
  )
}

test_that("local operations use the requested library", {
  old_paths <- .libPaths()
  first <- tempfile()
  second <- tempfile()
  dir.create(first)
  dir.create(second)
  first <- normalizePath(first, winslash = "/")
  second <- normalizePath(second, winslash = "/")
  on.exit(.libPaths(old_paths), add = TRUE)
  on.exit(unlink(c(first, second), recursive = TRUE), add = TRUE)

  make_library_test_package(first, "pacsTestFixture", "1.0", imports = "pacsTestFirstOnly", export = "first_export", news = "first NEWS")
  make_library_test_package(first, "pacsTestFirstOnly", "1.0")
  make_library_test_package(first, "pacsTestOutsideOne", "1.0", imports = "pacsTestDep")
  make_library_test_package(first, "pacsTestOutsideTwo", "1.0", imports = "pacsTestDep")
  make_library_test_package(second, "pacsTestFixture", "2.0", imports = "pacsTestDep", export = "second_export", news = "second NEWS")
  make_library_test_package(second, "pacsTestDep", "1.0")
  .libPaths(c(first, second, old_paths))

  expect_identical(pac_description("pacsTestFixture", local = TRUE, lib.loc = second)$Version, "2.0")
  expect_identical(pac_description("pacsTestFixture", version = "2.0", local = TRUE, lib.loc = second)$Version, "2.0")
  expect_identical(pac_news("pacsTestFixture", local = TRUE, lib.loc = second), "second NEWS")
  expect_identical(pac_namespace("pacsTestFixture", local = TRUE, lib.loc = second)$exports, "second_export")

  is_last <- pac_islast
  mockery::stub(is_last, "pac_isin", TRUE)
  mockery::stub(is_last, "pac_last", "2.0")
  expect_true(is_last("pacsTestFixture", lib.loc = second))

  lifetime <- pac_lifeduration
  mockery::stub(lifetime, "is_online", TRUE)
  mockery::stub(lifetime, "is_isin", TRUE)
  mockery::stub(lifetime, "pac_last", "2.0")
  mockery::stub(lifetime, "Sys.Date", as.Date("2020-02-01"))
  expect_equal(
    as.numeric(lifetime("pacsTestFixture", lib.loc = second, repos = "https://example.test")),
    31
  )

  heavy <- pac_deps_heavy("pacsTestFixture", local = TRUE, lib.loc = second, repos = "https://example.test")
  expect_identical(heavy$Package, "pacsTestDep")
  expected_size <- pac_size("pacsTestFixture", lib.loc = second) + pac_size("pacsTestDep", lib.loc = second)
  true_size <- pac_true_size
  dependencies <- pac_deps
  mockery::stub(true_size, "pac_deps", function(...) {
    dependencies(..., repos = "https://example.test")
  })
  expect_equal(true_size("pacsTestFixture", lib.loc = second, exclude_joint = 1L), expected_size)

  compare <- pac_compare_news
  mockery::stub(compare, "is_online", TRUE)
  mockery::stub(compare, "pac_isin", TRUE)
  mockery::stub(compare, "pac_last", function(pac, repos) {
    stopifnot(identical(repos, "https://example.test"))
    "3.0"
  })
  mockery::stub(compare, "pac_news", c("# Version 3.0", "new", "# Version 2.0", "old", "# Version 1.0"))
  expect_identical(
    compare("pacsTestFixture", lib.loc = second, repos = "https://example.test"),
    c("# Version 3.0", "new")
  )

  description <- pac_description
  compare_versions <- pac_compare_versions
  mockery::stub(compare_versions, "is_online", TRUE)
  mockery::stub(compare_versions, "pac_isin", TRUE)
  mockery::stub(compare_versions, "pac_last", function(pac, repos) {
    stopifnot(identical(repos, "https://example.test"))
    "3.0"
  })
  mockery::stub(compare_versions, "pac_description", function(pac, version = NULL, local = FALSE, lib.loc = .libPaths(), ...) {
    if (local) {
      description(pac, local = TRUE, lib.loc = lib.loc)
    } else {
      list(Version = version, Imports = "pacsTestDep (>= 1.0)")
    }
  })
  versions <- compare_versions("pacsTestFixture", lib.loc = second, repos = "https://example.test")
  expect_identical(attr(versions, "old"), "2.0")
  expect_identical(attr(versions, "new"), "3.0")

  compare_namespace <- pac_compare_namespace
  mockery::stub(compare_namespace, "is_online", TRUE)
  mockery::stub(compare_namespace, "pac_isin", TRUE)
  mockery::stub(compare_namespace, "pac_last", function(pac, repos) {
    stopifnot(identical(repos, "https://example.test"))
    "3.0"
  })
  mockery::stub(compare_namespace, "pac_namespace", function(pac, version, ...) {
    pac_parse_namespace(paste0("export(fn", gsub(".", "", version, fixed = TRUE), ")"), "UTF-8")
  })
  namespace <- compare_namespace("pacsTestFixture", lib.loc = second, repos = "https://example.test")
  expect_identical(attr(namespace, "old"), "2.0")
  expect_identical(attr(namespace, "new"), "3.0")
})

test_that("app operations use the requested library", {
  skip_if_not_installed("renv")

  old_paths <- .libPaths()
  first <- tempfile()
  second <- tempfile()
  dir.create(first)
  dir.create(second)
  first <- normalizePath(first, winslash = "/")
  second <- normalizePath(second, winslash = "/")
  on.exit(.libPaths(old_paths), add = TRUE)
  on.exit(unlink(c(first, second), recursive = TRUE), add = TRUE)

  make_library_test_package(first, "pacsTestFirstOnly", "1.0")
  make_library_test_package(second, "pacsTestFixture", "2.0", imports = "pacsTestDep")
  make_library_test_package(second, "pacsTestDep", "1.0")
  .libPaths(c(first, second, old_paths))

  app <- tempfile()
  missing_app <- tempfile()
  dir.create(app)
  dir.create(missing_app)
  on.exit(unlink(c(app, missing_app), recursive = TRUE), add = TRUE)
  writeLines("library(pacsTestFixture)", file.path(app, "app.R"))
  writeLines("library(pacsTestFirstOnly)", file.path(missing_app, "app.R"))
  expect_identical(
    app_deps(app, lib.loc = second, recursive = FALSE, repos = "https://example.test")$Package,
    "pacsTestFixture"
  )
  expect_error(
    app_deps(missing_app, lib.loc = second, recursive = FALSE, repos = "https://example.test"),
    "pacsTestFirstOnly"
  )

  expected_size <- pac_size("pacsTestFixture", lib.loc = second) + pac_size("pacsTestDep", lib.loc = second)
  size <- app_size
  mockery::stub(size, "app_deps", data.frame(Package = c("pacsTestFixture", "pacsTestDep")))
  expect_equal(size(app, lib.loc = second), expected_size + dir_size(app))
})
