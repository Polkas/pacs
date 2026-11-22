# Validate a specific local package

Checking if installed package dependencies have correct versions taking
into account their DESCRIPTION files requirements. Moreover identifying
which packages are newest releases. Optionally we could add life
duration and CRAN check page status for each dependency.

## Usage

``` r
pac_validate(
  pac,
  lib.loc = .libPaths(),
  fields = c("Depends", "Imports", "LinkingTo"),
  lifeduration = FALSE,
  checkred = list(scope = character(0), flavors = NULL),
  repos = biocran_repos()
)
```

## Arguments

- pac:

  `character` a package name.

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

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`data.frame` with 5/7/8/9 columns.

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

- direct:

  logical if the package is in the first dependency layer, direct
  dependencies from DESCRIPTION file.

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
When turn on the `lifeduration` option, calculations might be time
consuming when there is more than 500 packages. Please as a courtesy to
the R CRAN, don't overload their server by constantly using this
function with `lifeduration` or `checkred` turned on. Results are cached
with `memoise` package, memory cache. The `crandb` R packages database
is a part of `METACRAN` project, source: Csárdi G, Salmon M (2022).
`pkgsearch`: Search and Query CRAN R Packages.
`https://github.com/r-hub/pkgsearch`,
`https://r-hub.github.io/pkgsearch/`.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_validate("memoise")
pacs::pac_validate(
  "memoise",
  lifeduration = TRUE,
  checkred = list(scope = c("ERROR", "FAIL"), flavors = NULL)
)
pacs::pac_validate(
  "memoise",
  lifeduration = TRUE,
  checkred = list(scope = c("ERROR", "FAIL"), flavors = pacs::match_flavors())
)
} # }
```
