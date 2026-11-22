# Package dependencies

Package dependencies from DESCRIPTION files with installed or expected
versions or newest released.

## Usage

``` r
pac_deps(
  pac,
  fields = c("Depends", "Imports", "LinkingTo"),
  lib.loc = .libPaths(),
  base = FALSE,
  local = TRUE,
  description_v = FALSE,
  attr = TRUE,
  recursive = TRUE,
  repos = biocran_repos()
)
```

## Arguments

- pac:

  `character` a package name.

- fields:

  `character` vector listing the types of dependencies, a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
  Character string "all" is shorthand for that vector, character string
  "most" for the same vector without "Enhances", character string
  "strong" (default) for the first three elements of that vector.
  Default: `c("Depends", "Imports", "LinkingTo")`

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- base:

  `logical` if to add base packages too. If `TRUE` then
  [`pacs::pacs_base()`](https://polkas.github.io/pacs/reference/pacs_base.md)
  are taken into account. Default: `FALSE`

- local:

  logical if to use local repository (or newest remote packages).
  Default: TRUE

- description_v:

  `logical` if the dependencies version should be taken from description
  files, minimal required. By default installed versions are taken.
  Default: `FALSE`

- attr:

  `logical` if a package and its version should be added as an attribute
  of data.frame or for FALSE as an additional record. Default: TRUE

- recursive:

  `logical` if to assess the dependencies recursively. Default: TRUE

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`data.frame` with packages and their versions. Versions are taken from
`installed.packages` or newest released.

## Note

When function is invoked in the loop afterwards results could be
aggregated like,
`stats::aggregate(results[, c("Version"), drop = FALSE], list(Package = results$Package), pacs::compareVersionsMax)`.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_deps("stats", base = TRUE)$Package
pacs::pac_deps("memoise")$Package
pacs::pac_deps("memoise", description_v = FALSE)
# raw dependencies from DESCRIPTION file
pacs::pac_deps("memoise", description_v = TRUE, recursive = FALSE)
# raw dependencies from DESCRIPTION file - last release
pacs::pac_deps("memoise", description_v = TRUE, local = FALSE, recursive = FALSE)
} # }
```
