make_sysreq_package <- function(lib, name, version = "1.0.0",
                                depends = NULL, imports = NULL, linking_to = NULL,
                                suggests = NULL, enhances = NULL, requirements = NULL) {
  path <- file.path(lib, name)
  dir.create(path)
  dir.create(file.path(path, "Meta"))
  description <- c(
    paste("Package:", name),
    paste("Version:", version),
    "Title: Test fixture",
    "Description: An isolated test fixture.",
    "License: MIT",
    paste("Built: R", getRversion(), "; ; ;", .Platform$OS.type),
    if (!is.null(depends)) paste("Depends:", depends),
    if (!is.null(imports)) paste("Imports:", imports),
    if (!is.null(linking_to)) paste("LinkingTo:", linking_to),
    if (!is.null(suggests)) paste("Suggests:", suggests),
    if (!is.null(enhances)) paste("Enhances:", enhances),
    if (!is.null(requirements)) paste("SystemRequirements:", requirements)
  )
  writeLines(description, file.path(path, "DESCRIPTION"))
  saveRDS(
    list(DESCRIPTION = read.dcf(file.path(path, "DESCRIPTION"))[1L, ],
      Built = list(R = getRversion())),
    file.path(path, "Meta", "package.rds")
  )
}

make_sysreq_packages <- function(lib, root_version = "1.0.0",
                                 root_requirements = "C++17 and Make") {
  make_sysreq_package(lib, "pacsSysRoot",
    version = root_version,
    depends = "R (>= 4.1.0), pacsSysDepends",
    imports = "pacsSysDirect, pacsSysNoRequirement",
    linking_to = "pacsSysLinking", suggests = "pacsSysSuggested",
    enhances = "pacsSysEnhanced", requirements = root_requirements
  )
  make_sysreq_package(lib, "pacsSysDepends", requirements = "Boost")
  make_sysreq_package(lib, "pacsSysDirect",
    imports = "pacsSysTransitive", requirements = "libcurl (>= 7)"
  )
  make_sysreq_package(lib, "pacsSysLinking")
  make_sysreq_package(lib, "pacsSysNoRequirement", linking_to = "pacsSysTransitive")
  make_sysreq_package(lib, "pacsSysTransitive",
    imports = "pacsSysRoot", requirements = "libxml2"
  )
  make_sysreq_package(lib, "pacsSysSuggested",
    imports = "pacsSysOptional", suggests = "pacsSysSecondSuggest", requirements = "libgit2"
  )
  make_sysreq_package(lib, "pacsSysOptional", requirements = "CMake")
  make_sysreq_package(lib, "pacsSysEnhanced", requirements = "OpenSSL")
  make_sysreq_package(lib, "pacsSysSecondSuggest", requirements = "Pandoc")
}

make_sysreq_repository <- function(path, root_version = "1.0.0",
                                   root_requirements = "C++17 and Make",
                                   repo_path = "repository") {
  sources <- file.path(path, "sources")
  repo <- file.path(path, repo_path)
  contrib <- file.path(repo, "src", "contrib")
  dir.create(sources)
  dir.create(contrib, recursive = TRUE)
  make_sysreq_packages(sources, root_version, root_requirements)

  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(sources)
  for (package in list.dirs(".", recursive = FALSE, full.names = FALSE)) {
    version <- read.dcf(file.path(package, "DESCRIPTION"), fields = "Version")[1L, 1L]
    utils::tar(
      file.path(contrib, paste0(package, "_", version, ".tar.gz")),
      files = package, compression = "gzip", tar = "internal"
    )
  }
  tools::write_PACKAGES(contrib, type = "source")
  path <- normalizePath(repo, winslash = "/", mustWork = TRUE)
  url <- paste0("file://", if (.Platform$OS.type == "windows") "/", path)
  list(url = url, contrib = contrib, path = repo, sources = sources)
}

sysreq_fixture_metadata <- function(repository) {
  paths <- list.dirs(repository$sources, recursive = FALSE, full.names = TRUE)
  metadata <- do.call(rbind, lapply(paths, function(path) {
    read.dcf(
      file.path(path, "DESCRIPTION"),
      fields = c("Package", "Version", "SystemRequirements")
    )
  }))
  rownames(metadata) <- NULL
  metadata
}

test_that("local system requirements include direct or recursive dependencies", {
  lib <- tempfile()
  dir.create(lib)
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  make_sysreq_packages(lib)

  direct <- pac_system_requirements("pacsSysRoot", lib.loc = lib, recursive = FALSE)
  expected <- list(
    pacsSysRoot = "C++17 and Make",
    pacsSysDepends = "Boost",
    pacsSysDirect = "libcurl (>= 7)",
    pacsSysLinking = character(),
    pacsSysNoRequirement = character()
  )
  attr(expected, "versions") <- stats::setNames(rep("1.0.0", 5), names(expected))
  expect_identical(direct, expected)
  expect_identical(
    unique(unlist(direct, use.names = FALSE)),
    c("C++17 and Make", "Boost", "libcurl (>= 7)")
  )

  recursive <- pac_system_requirements("pacsSysRoot", lib.loc = lib)
  expect_identical(names(recursive), c(names(direct), "pacsSysTransitive"))
  expect_identical(recursive[["pacsSysTransitive"]], "libxml2")
  expect_false(anyDuplicated(names(recursive)) > 0)
  expect_identical(
    names(pac_system_requirements("pacsSysRoot", fields = character(), lib.loc = lib)),
    "pacsSysRoot"
  )
})

test_that("direct and recursive dependency fields are configured separately", {
  lib <- tempfile()
  dir.create(lib)
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  make_sysreq_packages(lib)

  direct <- pac_system_requirements(
    "pacsSysRoot", fields = c("Imports", "Suggests"), lib.loc = lib,
    recursive = FALSE
  )
  recursive <- pac_system_requirements(
    "pacsSysRoot", fields = c("Imports", "Suggests"), lib.loc = lib
  )
  expect_identical(names(direct), c(
    "pacsSysRoot", "pacsSysDirect", "pacsSysNoRequirement", "pacsSysSuggested"
  ))
  expect_identical(names(recursive), c(
    "pacsSysRoot", "pacsSysDirect", "pacsSysNoRequirement", "pacsSysOptional",
    "pacsSysSuggested", "pacsSysTransitive"
  ))
  most <- pac_system_requirements("pacsSysRoot", fields = "most", lib.loc = lib)
  expect_true("pacsSysSuggested" %in% names(most))
  expect_true("pacsSysOptional" %in% names(most))
  expect_false("pacsSysSecondSuggest" %in% names(most))
  expect_true("pacsSysSecondSuggest" %in% names(pac_system_requirements(
    "pacsSysRoot", fields = "most", recursive_fields = "most", lib.loc = lib
  )))
  expect_identical(
    names(pac_system_requirements(
      "pacsSysRoot", fields = "most", recursive_fields = character(), lib.loc = lib
    )),
    names(pac_system_requirements("pacsSysRoot", fields = "most", recursive = FALSE, lib.loc = lib))
  )
  expect_identical(
    names(pac_system_requirements("pacsSysRoot", fields = "Suggests", lib.loc = lib)),
    c("pacsSysRoot", "pacsSysOptional", "pacsSysSuggested")
  )
  expect_true("pacsSysEnhanced" %in%
    names(pac_system_requirements("pacsSysRoot", fields = "all", lib.loc = lib)))
  expect_error(pac_system_requirements("pacsSysRoot", fields = "invalid", lib.loc = lib))
  expect_error(pac_system_requirements("pacsSysRoot", recursive_fields = "invalid", lib.loc = lib))
})

test_that("local library precedence is respected without evaluating repositories", {
  first <- tempfile()
  second <- tempfile()
  dir.create(first)
  dir.create(second)
  on.exit(unlink(c(first, second), recursive = TRUE), add = TRUE)
  make_sysreq_package(first, "pacsSysRoot", version = "2.0.0", requirements = "first library")
  make_sysreq_packages(second)

  requirements <- pac_system_requirements
  mockery::stub(requirements, "biocran_repos", function() stop("Unexpected repository lookup"))
  first_result <- requirements("pacsSysRoot", lib.loc = c(first, second))
  second_result <- requirements("pacsSysRoot", lib.loc = second, recursive = FALSE)
  expect_identical(attr(first_result, "versions"), c(pacsSysRoot = "2.0.0"))
  expect_identical(first_result, list(pacsSysRoot = "first library"), ignore_attr = TRUE)
  expect_identical(second_result[["pacsSysRoot"]], "C++17 and Make")
})

test_that("missing dependencies and invalid inputs are reported", {
  lib <- tempfile()
  dir.create(lib)
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  make_sysreq_package(lib, "pacsSysMissingRoot", imports = "pacsSysMissing")

  expect_warning(
    result <- pac_system_requirements("pacsSysMissingRoot", lib.loc = lib),
    "pacsSysMissing.*cannot be assessed"
  )
  expect_identical(names(result), c("pacsSysMissingRoot", "pacsSysMissing"))
  expect_identical(unname(attr(result, "versions")), c("1.0.0", NA_character_))
  expect_identical(unname(result), list(character(), NA_character_), ignore_attr = TRUE)
  expect_error(pac_system_requirements("missing", lib.loc = lib), "not found")
  expect_error(pac_system_requirements("../bad", lib.loc = lib), "valid package name")
  expect_error(pac_system_requirements("pacsSysMissingRoot", lib.loc = lib, recursive = NA))
  expect_error(pac_system_requirements("pacsSysMissingRoot", lib.loc = lib, local = NA))
})

test_that("a missing transitive dependency is reported only when followed", {
  lib <- tempfile()
  dir.create(lib)
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  make_sysreq_package(lib, "pacsSysIncomplete", imports = "pacsSysBridge")
  make_sysreq_package(lib, "pacsSysBridge", imports = "pacsSysMissingGrandchild")

  expect_silent(direct <- pac_system_requirements(
    "pacsSysIncomplete", lib.loc = lib, recursive = FALSE
  ))
  expect_identical(names(direct), c("pacsSysIncomplete", "pacsSysBridge"))
  expect_warning(
    recursive <- pac_system_requirements("pacsSysIncomplete", lib.loc = lib),
    "pacsSysMissingGrandchild.*cannot be assessed"
  )
  expect_identical(names(recursive), c(
    "pacsSysIncomplete", "pacsSysBridge", "pacsSysMissingGrandchild"
  ))
  expect_identical(attr(recursive, "versions")[["pacsSysMissingGrandchild"]], NA_character_)
})

test_that("source repository requirements are read from package archives offline", {
  root <- tempfile("pacs sysreq ")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  repository <- make_sysreq_repository(root)

  index <- utils::available.packages(
    repos = repository$url, type = "source", fields = "SystemRequirements"
  )
  expect_true(is.na(index["pacsSysRoot", "SystemRequirements"]))

  direct <- pac_system_requirements(
    "pacsSysRoot", local = FALSE, repos = repository$url, recursive = FALSE
  )
  recursive <- pac_system_requirements(
    "pacsSysRoot", local = FALSE, repos = repository$url
  )
  expect_identical(names(direct), c(
    "pacsSysRoot", "pacsSysDepends", "pacsSysDirect",
    "pacsSysLinking", "pacsSysNoRequirement"
  ))
  expect_identical(unname(direct), list(
    "C++17 and Make", "Boost", "libcurl (>= 7)", character(), character()
  ), ignore_attr = TRUE)
  expect_identical(names(recursive), c(names(direct), "pacsSysTransitive"))
  expect_identical(recursive[["pacsSysTransitive"]], "libxml2")
  expect_true("pacsSysOptional" %in% names(pac_system_requirements(
    "pacsSysRoot", fields = c("Imports", "Suggests"), local = FALSE,
    repos = repository$url
  )))
})

test_that("CRAN-style bulk metadata avoids downloading source archives", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  repository <- make_sysreq_repository(root)
  metadata <- sysreq_fixture_metadata(repository)
  metadata <- rbind(metadata, metadata[metadata[, "Package"] == "pacsSysRoot", , drop = FALSE])
  metadata_file <- file.path(repository$path, "web", "packages", "packages.rds")
  dir.create(dirname(metadata_file), recursive = TRUE)
  saveRDS(metadata, metadata_file)
  archives <- list.files(repository$contrib, pattern = "[.]tar[.]gz$", full.names = TRUE)
  expect_true(all(file.remove(archives)))

  result <- pac_system_requirements(
    "pacsSysRoot", fields = "most", local = FALSE, repos = repository$url
  )
  expect_identical(result[["pacsSysRoot"]], "C++17 and Make")
  expect_identical(result[["pacsSysSuggested"]], "libgit2")
  expect_identical(result[["pacsSysNoRequirement"]], character())
  expect_true("pacsSysOptional" %in% names(result))
  expect_false("pacsSysEnhanced" %in% names(result))
  expect_false("pacsSysSecondSuggest" %in% names(result))
})

test_that("Bioconductor bulk metadata avoids downloading source archives", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  repository <- make_sysreq_repository(root, repo_path = file.path("packages", "3.22", "bioc"))
  metadata <- sysreq_fixture_metadata(repository)
  records <- lapply(seq_len(nrow(metadata)), function(i) {
    record <- as.list(metadata[i, ])
    record[!vapply(record, is.na, logical(1))]
  })
  names(records) <- metadata[, "Package"]
  metadata_file <- file.path(root, "packages", "json", "3.22", "bioc", "packages.json")
  dir.create(dirname(metadata_file), recursive = TRUE)
  jsonlite::write_json(records, metadata_file, auto_unbox = TRUE)
  archives <- list.files(repository$contrib, pattern = "[.]tar[.]gz$", full.names = TRUE)
  expect_true(all(file.remove(archives)))

  result <- pac_system_requirements(
    "pacsSysRoot", fields = "most", local = FALSE, repos = repository$url
  )
  expect_identical(result[["pacsSysRoot"]], "C++17 and Make")
  expect_identical(result[["pacsSysSuggested"]], "libgit2")
  expect_identical(result[["pacsSysNoRequirement"]], character())
  expect_true("pacsSysOptional" %in% names(result))
  expect_false("pacsSysSecondSuggest" %in% names(result))
})

test_that("stale bulk metadata falls back to the selected source version", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  repository <- make_sysreq_repository(root)
  metadata <- sysreq_fixture_metadata(repository)
  metadata[metadata[, "Package"] == "pacsSysRoot", "Version"] <- "9.0.0"
  metadata[metadata[, "Package"] == "pacsSysRoot", "SystemRequirements"] <- "wrong version"
  metadata_file <- file.path(repository$path, "web", "packages", "packages.rds")
  dir.create(dirname(metadata_file), recursive = TRUE)
  saveRDS(metadata, metadata_file)

  result <- pac_system_requirements(
    "pacsSysRoot", fields = character(), local = FALSE, repos = repository$url
  )
  expect_identical(unname(attr(result, "versions")), "1.0.0")
  expect_identical(result[["pacsSysRoot"]], "C++17 and Make")
})

test_that("conflicting bulk metadata falls back to source archives", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  repository <- make_sysreq_repository(root)
  metadata <- sysreq_fixture_metadata(repository)
  duplicate <- metadata[metadata[, "Package"] == "pacsSysRoot", , drop = FALSE]
  duplicate[, "SystemRequirements"] <- "conflicting requirement"
  metadata <- rbind(metadata, duplicate)
  metadata_file <- file.path(repository$path, "web", "packages", "packages.rds")
  dir.create(dirname(metadata_file), recursive = TRUE)
  saveRDS(metadata, metadata_file)

  expect_warning(
    result <- pac_system_requirements(
      "pacsSysRoot", fields = character(), local = FALSE, repos = repository$url
    ),
    "conflicting requirements"
  )
  expect_identical(result[["pacsSysRoot"]], "C++17 and Make")
})

test_that("invalid bulk metadata is reported instead of ignored", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  repository <- make_sysreq_repository(root)
  metadata_file <- file.path(repository$path, "web", "packages", "packages.rds")
  dir.create(dirname(metadata_file), recursive = TRUE)
  saveRDS(matrix("invalid"), metadata_file)

  expect_error(
    pac_system_requirements(
      "pacsSysRoot", fields = character(), local = FALSE, repos = repository$url
    ),
    "Invalid bulk package metadata"
  )
})

test_that("source metadata comes from the repository selected by R", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  first <- file.path(root, "first")
  second <- file.path(root, "second")
  dir.create(first)
  dir.create(second)
  first_repo <- make_sysreq_repository(first)
  second_repo <- make_sysreq_repository(second,
    root_version = "2.0.0", root_requirements = "new source requirement"
  )

  result <- pac_system_requirements(
    "pacsSysRoot", fields = character(), local = FALSE,
    repos = c(first_repo$url, second_repo$url)
  )
  expect_identical(unname(attr(result, "versions")), "2.0.0")
  expect_identical(result[["pacsSysRoot"]], "new source requirement")
})

test_that("missing repository dependencies are reported as incomplete", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  repository <- make_sysreq_repository(root)
  unlink(file.path(repository$contrib, "pacsSysTransitive_1.0.0.tar.gz"))
  tools::write_PACKAGES(repository$contrib, type = "source")

  expect_warning(
    result <- pac_system_requirements("pacsSysRoot", local = FALSE, repos = repository$url),
    "pacsSysTransitive.*cannot be assessed"
  )
  expect_identical(attr(result, "versions")[["pacsSysTransitive"]], NA_character_)
  expect_identical(result[["pacsSysTransitive"]], NA_character_)
})

test_that("unreadable repository archives fail instead of hiding requirements", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  repository <- make_sysreq_repository(root)
  unlink(file.path(repository$contrib, "pacsSysRoot_1.0.0.tar.gz"))

  expect_error(
    suppressWarnings(pac_system_requirements(
      "pacsSysRoot", fields = character(), local = FALSE, repos = repository$url
    )),
    "Could not read DESCRIPTION for pacsSysRoot"
  )
})

test_that("repository metadata must match its source archive", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  repository <- make_sysreq_repository(root)
  expect_true(file.copy(
    file.path(repository$contrib, "pacsSysRoot_1.0.0.tar.gz"),
    file.path(repository$contrib, "pacsSysRoot_9.0.0.tar.gz")
  ))
  contrib <- paste0(repository$url, "/src/contrib")

  expect_error(
    repository_system_requirements_raw("pacsSysRoot", "9.0.0", contrib),
    "does not match the repository index"
  )
  expect_error(
    repository_system_requirements_raw("../bad", "1.0.0", contrib),
    "Invalid package name"
  )
  expect_error(
    repository_system_requirements_raw("pacsSysRoot", "../bad", contrib),
    "Invalid repository version"
  )
})

test_that("system requirements are split into a character vector", {
  expect_identical(
    split_system_requirements("GDAL (>= 2.0.1), GEOS (>= 3.4.0), PROJ (>= 4.8.0),\n   sqlite3"),
    c("GDAL (>= 2.0.1)", "GEOS (>= 3.4.0)", "PROJ (>= 4.8.0)", "sqlite3")
  )
  expect_identical(
    split_system_requirements("libcurl (>= 7.73): libcurl-devel (rpm) or\n libcurl4-openssl-dev (deb)"),
    "libcurl (>= 7.73): libcurl-devel (rpm) or libcurl4-openssl-dev (deb)"
  )
  expect_identical(split_system_requirements("GNU make; pandoc (a, b)"), c("GNU make", "pandoc (a, b)"))
  prose <- "On a Unix-alike, one of mach_absolute_time (macOS), clock_gettime or gethrtime. If none is found, gettimeofday is used."
  expect_identical(split_system_requirements(prose), prose)
  expect_identical(split_system_requirements(NA_character_), character())
  expect_identical(split_system_requirements(" , "), character())
})
