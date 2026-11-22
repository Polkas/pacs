# R CRAN package dependencies for a certain version or time point

Package dependencies from DESCRIPTION files retrieved recursively for
certain version or time point.

## Usage

``` r
pac_deps_timemachine(
  pac,
  version = NULL,
  at = NULL,
  fields = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)
```

## Arguments

- pac:

  `character` a package name.

- version:

  `character` version of a package. Default: `NULL`

- at:

  `Date` from which to take the version. Default: `NULL`

- fields:

  `character` vector listing the types of dependencies, a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
  Character string "all" is shorthand for that vector, character string
  "most" for the same vector without "Enhances", character string
  "strong" (default) for the first three elements of that vector.
  Default: `c("Depends", "Imports", "LinkingTo")`

- recursive:

  `logical` if to assess the dependencies recursively. Default: TRUE

## Value

`named vector` package dependencies and their versions at the release
date of main package plus one day.

## Note

Longer lived version is taken if 2 is available at the same date (switch
time).

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_deps_timemachine("memoise", "0.2.1")
pacs::pac_deps_timemachine("memoise", at = as.Date("2019-01-01"))
pacs::pac_deps_timemachine("dplyr", at = as.Date("2015-01-01"))
} # }
```
