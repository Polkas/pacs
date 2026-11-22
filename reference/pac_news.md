# Get NEWS for a package

Get NEWS for a package from CRAN or local

## Usage

``` r
pac_news(
  pac,
  version = NULL,
  at = NULL,
  local = FALSE,
  lib.loc = .libPaths(),
  repos = "https://cran.rstudio.com/"
)
```

## Arguments

- pac:

  `character` a package name.

- version:

  `character` version of a package. Default: `NULL`

- at:

  `Date` from which to take the version. Default: `NULL`

- local:

  `logical` if to use local repository (or newest remote packages).
  Default: `FALSE`

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- repos:

  `character` vector repositories URLs to use. Used only for the
  validation. Default `https://cran.rstudio.com/`

## Value

`character` with NEWS content.

## Note

Results are cached for 30 minutes with `memoise` package.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_news("dplyr", version = "0.8.0")
pacs::pac_news("dplyr", at = as.Date("2019-02-01"))
} # }
```
