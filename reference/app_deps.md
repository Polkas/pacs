# The shiny app dependencies

the shiny app dependencies packages are checked recursively. The
`c("Depends", "Imports", "LinkingTo")` DESCRIPTION files fields are
checked recursively. The required dependencies have to be installed in
the local repository. The default arguments setup is recommended.

## Usage

``` r
app_deps(
  path = ".",
  fields = c("Depends", "Imports", "LinkingTo"),
  lib.loc = .libPaths(),
  local = TRUE,
  base = FALSE,
  description_v = FALSE,
  recursive = TRUE,
  repos = biocran_repos()
)
```

## Arguments

- path:

  `character` path to the shiny app. Default: `"."`

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

- local:

  `logical` if to use local repository (or newest remote packages).
  Default: TRUE

- base:

  `logical` if to add base packages too. If `TRUE` then
  [`pacs::pacs_base()`](https://polkas.github.io/pacs/reference/pacs_base.md)
  are taken into account. Default: `FALSE`

- description_v:

  `logical` if the dependencies version should be taken from description
  files, minimal required. By default installed versions are taken.
  Default: `FALSE`

- recursive:

  `logical` if to assess the dependencies recursively. Default: TRUE

- repos:

  `character` vector base URLs of the repositories to use. By default
  checking CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`character` vector with dependency packages or data.frame when checking
recursively.

## Note

`renv` package has to be installed.

## Examples

``` r
if (FALSE) { # \dontrun{
library(renv)
# Please update the path to the custom shiny app
app_path <- system.file("examples/04_mpg", package = "shiny")
pacs::app_deps(app_path)
pacs::app_deps(app_path, recursive = FALSE)
} # }
```
