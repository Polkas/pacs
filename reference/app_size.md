# Size of the shiny app

The size of shiny app is a sum of dependencies packages and the app
directory. The app dependencies packages are checked recursively, and
only in local repository. The default arguments setup is recommended.

## Usage

``` r
app_size(
  path = ".",
  fields = c("Depends", "Imports", "LinkingTo"),
  lib.loc = .libPaths(),
  recursive = TRUE
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

- recursive:

  `logical` if to assess the dependencies recursively. Default: TRUE

## Value

`numeric` size in bytes, or `NA` if dependencies cannot be determined.
To get MB, divide by `10**6`.

## Note

`renv` package has to be installed. `base` packages
([`pacs::pacs_base()`](https://polkas.github.io/pacs/reference/pacs_base.md))
are not taken into account.

## Examples

``` r
if (FALSE) { # \dontrun{
library(renv)
# Please update the path to the shiny app
cat(pacs::app_size(system.file("examples/04_mpg", package = "shiny")) / 10**6, "MB")
} # }
```
