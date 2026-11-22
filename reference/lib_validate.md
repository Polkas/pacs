# Validate the local library

Checking if installed packages have correct versions taking into account
all DESCRIPTION files requirements. Moreover identifying which packages
are newest releases. Optionally we could add life duration and CRAN
check page status for each package.

## Usage

``` r
lib_validate(
  lib.loc = .libPaths(),
  fields = c("Depends", "Imports", "LinkingTo"),
  lifeduration = FALSE,
  checkred = list(scope = character(0), flavors = NULL),
  built = FALSE,
  repos = biocran_repos()
)
```

## Arguments

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- fields:

  `character` vector listing the types of dependencies, a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
  Character string "all" is shorthand for that vector, character string
  "most" for the same vector without "Enhances", character string
  "strong" (default) for the first three elements of that vector.
  Default: `c("Depends", "Imports", "LinkingTo")`

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

- built:

  `logical` if to add an R version under which each package was
  installed. Useful mainly for a local usage. Packages installed with a
  previous version of R could not work correctly with the new version
  of R. Default: `FALSE`

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`data.frame` with 4/6/8/9/10 columns.

- Package:

  character a package name.

- Version.expected.min:

  character expected by DESCRIPTION files minimal version. "" means not
  specified so the newest version.

- Version.have:

  character installed package version.

- version_status:

  numeric -1/0/1 which comes from
  [`utils::compareVersion`](https://rdrr.io/r/utils/compareVersion.html)
  function. 0 means that we have the same version as required by
  DESCRIPTION files. -1 means we have too low version installed, this is
  an error. 1 means we have higher version.

- built:

  character package was built under this R version

- built_status:

  integer if the package was built under the current R version, then 1
  (good) and for older R versions 0 (possibly bad). A package built
  under older R version or mix of packages built under different
  versions could bring possible failures.

- newest:

  logical (Internet needed) if the installed version is the newest one.
  For Bioconductor if is the newest one per R version.

- cran:

  logical (Internet needed) if the package is on CRAN, version is not
  taken into account here.

- checkred:

  (Optional) (Internet needed) logical if the NEWEST package contains
  any specified statuses on CRAN check page.
  [`pacs::checked_packages`](https://polkas.github.io/pacs/reference/checked_packages.md)
  is used to quickly retrieve all statuses at once.

- lifeduration:

  (Optional) (Internet needed) integer number of days a package was
  released.

## Note

Version.expected.min column not count packages which are not a
dependency for any package, so could not be find in DESCRIPTION files.
When turn on the `lifeduration` options, calculations might be time
consuming for libraries bigger than 500 packages. Results are cached for
30 minutes with `memoise` package. `BioConductor` packages are tested
only in available scope, `checkred` is not assessed for them. The
`crandb` R packages database is a part of `METACRAN` project, source:
Csárdi G, Salmon M (2022). `pkgsearch`: Search and Query CRAN R
Packages. `https://github.com/r-hub/pkgsearch`,
`https://r-hub.github.io/pkgsearch/`.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::lib_validate()
pacs::lib_validate(checkred = list(scope = c("ERROR", "FAIL", "WARN")))
pacs::lib_validate(checkred = list(
  scope = c("ERROR", "FAIL"),
  flavors = pacs::match_flavors()
))
# activate lifeduration argument, could be time consuming for bigger libraries.
pacs::lib_validate(
  lifeduration = TRUE,
  checkred = list(scope = c("ERROR", "FAIL"))
)
# only R CRAN repository
pacs::lib_validate(repos = "https://cran.rstudio.com/")
} # }
```
