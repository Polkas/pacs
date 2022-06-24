test_that("pacs::pac_deps", {
  stats_deps <- pacs::pac_deps("stats", base = TRUE)
  stats_deps_tools <- tools::package_dependencies("stats",
    recursive = TRUE,
    db = installed.packages()
  )[[1]]
  expect_true(ncol(stats_deps) > 0)
  expect_true(length(stats_deps$Package) > 0 && length(stats_deps_tools) > 0)
  stats_deps_attr <- attributes(stats_deps)
  expect_true(stats_deps_attr$Package == "stats")
  expect_true(stats_deps_attr$class == "data.frame")
  stats_deps2 <- pacs::pac_deps("stats", attr = FALSE, base = TRUE)
  expect_true(ncol(stats_deps2) > 0)
  expect_true(length(stats_deps2$Package) > 0)
  expect_true(ncol(pacs::pac_deps("memoise", description_v = TRUE, recursive = FALSE)) == 2)
})

test_that("pacs::pac_deps cols online", {
  skip_if_offline()
  expect_true(ncol(pacs::pac_deps("memoise",
    description_v = TRUE,
    recursive = FALSE,
    local = FALSE,
    repos = "https://cran.rstudio.com/"
  )) == 2)
})

test_that("pacs::pac_deps", {
  skip_if_offline()
  deps1 <- pacs::pac_deps("memoise",
    description_v = TRUE,
    recursive = FALSE,
    local = FALSE,
    repos = "https://cran.rstudio.com/"
  )
  deps2 <- setdiff(
    tools::package_dependencies("memoise",
      db = available_packages(repos = "https://cran.rstudio.com/"),
      recursive = FALSE
    )[[1]],
    pacs::pacs_base()
  )

  expect_true(nrow(deps1) == length(deps2))
})

test_that("pacs::pac_deps recursive", {
  skip_if_offline()
  deps1_recursive <- pacs::pac_deps("memoise",
    description_v = TRUE,
    recursive = TRUE,
    local = FALSE,
    repos = "https://cran.rstudio.com/"
  )
  deps2_recursive <- setdiff(
    tools::package_dependencies("memoise",
      db = available_packages(repos = "https://cran.rstudio.com/"),
      recursive = TRUE
    )[[1]],
    pacs::pacs_base()
  )
  expect_true(nrow(deps1_recursive) == length(deps2_recursive))
})

test_that("pacs::pac_deps recursive long field", {
  skip_if_offline()
  deps3_recursive <- pacs::pac_deps("memoise",
    description_v = FALSE,
    fields = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
    recursive = TRUE,
    local = FALSE,
    repos = "https://cran.rstudio.com/"
  )
  deps4_recursive <- setdiff(
    tools::package_dependencies("memoise",
      which = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
      db = available_packages(repos = "https://cran.rstudio.com/"),
      recursive = TRUE
    )[[1]],
    pacs::pacs_base()
  )
  expect_true(nrow(deps3_recursive) == length(deps4_recursive))
})

test_that("pacs::pac_deps_timemachine", {
  skip_if_offline()
  expect_identical(pac_deps_timemachine("WRONG", "0.8.0"), NA)
  expect_true(length(pac_deps_timemachine("memoise", "0.2.1")) == 1)
  expect_true(length(pac_deps_timemachine("memoise", at = as.Date("2019-01-01"))) == 1)
})

test_that("pacs::pac_deps_timemachine offline", {
  pac_deps_timemachine_offline <- pac_deps_timemachine
  mockery::stub(pac_deps_timemachine_offline, "is_online", FALSE)
  expect_identical(pac_deps_timemachine_offline("dplyr", "0.8.0"), NA)
})

test_that("pacs::app_deps", {
  skip_if_offline()
  rec_deps <- nrow(pacs::app_deps("files/shiny_app"))
  direct_deps <- nrow(pacs::app_deps("files/shiny_app", recursive = FALSE))
  expect_true(rec_deps > 0)
  expect_true(direct_deps > 0)
  expect_true(rec_deps >= direct_deps)
  expect_error(pacs::app_deps("WRONG"))
  expect_error(pacs::app_deps("files/shiny_app", 12))
})

test_that("pac_deps_user", {
  skip_if_offline()
  pp <- pacs::pac_deps_user("memoise", base = FALSE, attr = FALSE, repos = "https://cran.rstudio.com/")$Package
  rr <- remotes:::find_deps("memoise",
                            available = pacs:::available_packages(repos = "https://cran.rstudio.com/"),
                            top_dep = NA)
  rrr <- setdiff(rr, pacs::pacs_base())
  expect_identical(sort(pp), sort(rrr))

  expect_true(isNA(pac_deps_user("WRONG")))
})

test_that("pac_deps_dev", {
  skip_if_offline()
  pp <- pacs::pac_deps_dev("memoise", base = FALSE, attr = FALSE, repos = "https://cran.rstudio.com/")$Package
  rr <- remotes:::find_deps("memoise",
                            available = pacs:::available_packages(repos = "https://cran.rstudio.com/"),
                            top_dep = TRUE)
  rrr <- setdiff(rr, pacs::pacs_base())
  expect_identical(sort(pp), sort(rrr))

  expect_true(isNA(pac_deps_dev("WRONG")))
  expect_true(nrow(pac_deps_dev("tinytest", repos = "https://cran.rstudio.com/")) >= 0)
})

test_that("pac_deps_heavy", {
  skip_if_offline()
  expect_identical(
    pac_deps_heavy("pacs"),
    vapply(lapply(tools::package_dependencies(pac_deps("pacs", local = FALSE, recursive = FALSE)$Package, recursive = TRUE, db = available_packages(repos = biocran_repos())), function(x) setdiff(x, pacs_base())), length, integer(1))
  )
})

test_that("pac_deps_heavy", {
  expect_identical(
    pac_deps_heavy("memoise", local = TRUE, base = FALSE),
    vapply(lapply(tools::package_dependencies(pac_deps("memoise", local = TRUE, recursive = FALSE)$Package, recursive = TRUE, db = installed_packages(lib.loc = .libPaths())), function(x) setdiff(x, pacs_base())), length, integer(1))
  )
})

test_that("pac_deps_heavy with base", {
  expect_true(length(pac_deps_heavy("memoise", local = TRUE, base = TRUE)) >= length(pac_deps_heavy("memoise", local = TRUE)))
})

test_that("pac_deps_heavy 0 deps pac", {
  expect_identical(
    pac_deps_heavy("base", local = TRUE),
    structure(integer(0), names = character(0))
  )
})

test_that("pac_deps_heavy WRONG", {
  skip_if_offline()
  expect_error(pac_deps_heavy("WRONG", local = TRUE))
  expect_true(isNA(pac_deps_heavy("WRONG", local = FALSE)))
})
