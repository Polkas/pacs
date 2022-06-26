#' Standard Template Arguments
#'
#' @param at `Date` from which to take the version. Default: `NULL`
#' @param attr `logical` specify if a package and its version should be added as an attribute of data.frame or for FALSE as an additional record. Default: TRUE
#' @param base `logical` if to add base packages too. If `TRUE` then `pacs::pacs_base()` are taken into account. Default: `FALSE`
#' @param built `logical` if to add an R version under which each package was installed.
#' Useful mainly for a local usage.
#' Packages installed with a previous version of R could not work correctly with the new version of R. Default: `FALSE`
#' @param checkred `list` with two named fields, `scope` and `flavor`. `scope` of R CRAN check pages statuses to consider, any of `c("ERROR", "FAIL", "WARN", "NOTE")`.
#' `flavor` is a vector of CRAN machines to consider, which might be retrieved with `pacs::cran_flavors()$Flavor`.
#' By default an empty scope field deactivated assessment for `checkred` column, and NULL flavor will results in checking all machines.
#' Default `list(scope = character(0), flavor = NULL)`
#' @param description_v `logical` if the dependencies version should be taken from description files, minimal required. By default installed versions are taken. Default: `FALSE`
#' @param exclude_joint `integer` exclude packages which are dependencies of at least N other packages, not count main package dependencies. Default: `0`
#' @param fields `character` vector listing the types of dependencies, a subset of `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
#' Character string "all" is shorthand for that vector, character string "most" for the same vector without "Enhances", character string "strong" (default) for the first three elements of that vector.
#' Default: `c("Depends", "Imports", "LinkingTo")`
#' @param flavors `character` vector of CRAN server names to consider, possible names could be get with `pacs::cran_flavors()$Flavor`.
#' The `pacs::match_flavors()` function could be used to get CRAN server names matched for your local `OS`.
#' By default all CRAN machines are considered `NULL` value. Default `NULL`
#' @param from `Date` the lower limit. Default: `NULL`
#' @param lib.loc `character` vector of search paths for local packages. Default: `.libPaths()`
#' @param lifeduration `logical` if to assess life duration for each package in the library.
#' `MEATCRAN CRANDB` is used for libraries with less than 500 packages.
#' The direct web page download from CRAN or local evaluation for newest packages otherwise. Default: `FALSE``
#' @param limit `numeric` at least days to treat as healthy, ">=limit". Default: 14
#' @param local `logical` if to use local repository (or newest remote packages). Default: `FALSE`
#' @param na.rm `logical` if to remove `NA` values.
#' @param new `character` a new version of package, default newest version. Default: `NULL`
#' @param old `character` an old version of package, default local version. Default: `NULL`
#' @param pac `character` a package name.
#' @param pacs `character` vector of packages names.
#' @param path `character` path to the shiny app. Default: `"."`
#' @param repos `character` vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' @param recursive `logical` if to assess the dependencies recursively. Default: TRUE
#' @param scope `character` vector scope of the check, accepted values c("ERROR", "FAIL", "WARN", "NOTE"). Default `c("ERROR", "FAIL")`
#' @param source `character`` one of `c("crandb", "cran")`. Using the `MEATCRAN CRANDB` or the direct web page download from CRAN. Default: `"crandb"`
#' @param startup `logical` include only `startup` packages. Default: `FALSE`
#' @param to `Date` the upper limit. Default: `NULL`
#' @param vec `character` vector.
#' @param version `character` version of a package. Default: `NULL`
#' @param versions `character` vector of packages versions.
#' @name standard_args
#' @keywords internal
#'
NULL
