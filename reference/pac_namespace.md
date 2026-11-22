# package NAMESPACE file

CRAN package NAMESPACE file taken locally or remotely from GITHUB CRAN
mirror or CRAN website. By default works for the newest package version.

## Usage

``` r
pac_namespace(
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

`list` with names proper for NAMESPACE file, the same as format as
returned by
[`base::parseNamespaceFile`](https://rdrr.io/r/base/ns-internal.html).

## Note

Results are cached for 30 minutes with `memoise` package. This function
is mainly built under source code from
[`base::parseNamespaceFile`](https://rdrr.io/r/base/ns-internal.html).

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_namespace("dplyr", version = "0.8.0")
pacs::pac_namespace("dplyr", at = as.Date("2019-02-01"))
pacs::pac_namespace("memoise", local = TRUE)
} # }
```
