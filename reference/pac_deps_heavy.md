# Package direct dependencies and number of dependencies for each of them

A higher-level function, build from `pacs::pacs_deps` and
[`tools::package_dependencies`](https://rdrr.io/r/tools/package_dependencies.html).
A tool to identify a main sources of dependencies, which direct
dependencies are the heaviest one.

## Usage

``` r
pac_deps_heavy(
  pac,
  fields = c("Depends", "Imports", "LinkingTo"),
  lib.loc = .libPaths(),
  base = FALSE,
  local = FALSE,
  repos = pacs::biocran_repos()
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

  `logical` if to use local repository (or newest remote packages).
  Default: `FALSE`

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`data.frame` with three columns
`c("Package", "NrDeps", "NrUniqueDeps")`: package name, number of
dependencies and number of unique dependencies (not shared by other
direct dependencies).

## Note

Please take into account that the sum of the dependencies is not equal
to the number of dependencies of the main package, because some
dependencies are overlapping.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_deps_heavy("caret")
pacs::pac_deps_heavy("dplyr")
} # }
```
