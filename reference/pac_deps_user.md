# Package dependencies - user perspective

A higher-level function, build from `pacs::pacs_deps`. Package
dependencies installed when run `installed.packages`.
`"Depends", "Imports", "LinkingTo"` fields from the DESCRIPTION file and
their recursive dependencies taken from the same fields. Dependencies
are taken remotely for the newest version.

## Usage

``` r
pac_deps_user(
  pac,
  base = FALSE,
  local = FALSE,
  attr = TRUE,
  repos = pacs::biocran_repos()
)
```

## Arguments

- pac:

  `character` a package name.

- base:

  `logical` if to add base packages too. If `TRUE` then
  [`pacs::pacs_base()`](https://polkas.github.io/pacs/reference/pacs_base.md)
  are taken into account. Default: `FALSE`

- local:

  `logical` if to use local repository (or newest remote packages).
  Default: `FALSE`

- attr:

  `logical` if a package and its version should be added as an attribute
  of data.frame or for FALSE as an additional record. Default: TRUE

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`data.frame` with packages and their versions. Versions are taken from
`installed.packages` or newest released.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_deps_user("dplyr")
pacs::pac_deps_user("pacs")
# with the main package in the list
pacs::pac_deps_user("pacs", attr = FALSE)
} # }
```
