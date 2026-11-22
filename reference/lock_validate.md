# Validate a specific renv lock file

This function will be especially useful when renv lock file is built
manually. Checking if packages in the lock file have correct versions
taking into account their DESCRIPTION files requirements
(`c("Depends", "Imports", "LinkingTo")`). Moreover identifying which
packages are newest releases. Optionally we could add life duration and
CRAN check page status for each dependency.

## Usage

``` r
lock_validate(
  path,
  lifeduration = FALSE,
  checkred = list(scope = character(0), flavors = NULL),
  lib.loc = .libPaths(),
  repos = biocran_repos()
)
```

## Arguments

- path:

  `character` path to the shiny app. Default: `"."`

- lifeduration:

  `logical` if to assess life duration for each package in the library.
  For installed newest releases of packages, a local evaluation is used.
  `MEATCRAN CRANDB` is used for libraries with less than 500 packages.
  Otherwise the direct web page download from CRAN is used. Default:
  \`FALSE“

- checkred:

  `list` with two named fields, `scope` and `flavor`. `scope` of R CRAN
  check pages statuses to consider, any of
  `c("ERROR", "FAIL", "WARN", "NOTE")`. `flavor` is a vector of CRAN
  machines to consider, which might be retrieved with
  `pacs::cran_flavors()$Flavor`. By default an empty scope field
  deactivated assessment for `checkred` column, and NULL flavor will
  results in checking all machines. Default:
  `list(scope = character(0), flavor = NULL)`

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`data.frame` with 2/6/7/8 columns.

- Package:

  character a package name.

- Version.expected.min:

  (conditional) (Internet needed) character expected by DESCRIPTION
  files minimal version. "" means not specified so the newest version.

- Version.expected:

  character package version in the renv lock file.

- version_status:

  (conditional) numeric -1/0/1 which comes from
  [`utils::compareVersion`](https://rdrr.io/r/utils/compareVersion.html)
  function. 0 means that we have the same version as required by
  DESCRIPTION files. -1 means we have too low version installed, this is
  an error. 1 means we have higher version.

- newest:

  logical (Internet needed) if the installed version is the newest one.

- cran:

  logical (Internet needed) if the package is on CRAN, version is not
  taken into account here.

- checkred:

  (Optional) (Internet needed) logical if the NEWEST package contains
  any specified statuses on CRAN check page.

- lifeduration:

  (Optional) (Internet needed) integer number of days a package was
  released.

## Note

Version.expected.min column not count packages which are not a
dependency for any package, so could not be find in DESCRIPTION files.
`Version.expected.min` and `version_status` are assessed only if there
are less than 500 packages in the lock file. When turn on the
`lifeduration` option, calculations might be time consuming when there
is more than 500 packages. The `crandb` R packages database is a part of
`METACRAN` project, source: Csárdi G, Salmon M (2022). `pkgsearch`:
Search and Query CRAN R Packages. `https://github.com/r-hub/pkgsearch`,
`https://r-hub.github.io/pkgsearch/`.

## Examples

``` r
if (FALSE) { # \dontrun{
# path or url
url <- "https://raw.githubusercontent.com/Polkas/pacs/master/tests/testthat/files/renv_test.lock"
pacs::lock_validate(url)

pacs::lock_validate(
  url,
  checkred = list(scope = c("ERROR", "FAIL"), flavors = pacs::match_flavors())
)

pacs::lock_validate(
  url,
  lifeduration = TRUE,
  checkred = list(scope = c("ERROR", "FAIL"), flavors = NULL)
)
} # }
```
