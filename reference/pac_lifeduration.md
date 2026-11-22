# Package version life duration at specific Date or for a specific version

a package life duration for a certain version or at a specific Date. By
default works for the newest package version.

## Usage

``` r
pac_lifeduration(
  pac,
  version = NULL,
  at = NULL,
  lib.loc = .libPaths(),
  repos = biocran_repos(),
  source = c("cran", "crandb")
)
```

## Arguments

- pac:

  `character` a package name.

- version:

  `character` version of a package. Default: `NULL`

- at:

  `Date` from which to take the version. Default: `NULL`

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

- source:

  `character` one of `c("cran", "crandb")`. Using the `MEATCRAN CRANDB`
  or the direct web page download from CRAN. When `"crandb"` is set then
  `options(pacs.crandb_ntry = 3)` and
  `options(pacs.crandb_nsleep = 0.001)` can be used to control the
  fetch. `"pacs.crandb_ntry"` can be used to set the maximum number of
  try outs, by default 3. `"pacs.crandb_nsleep"` can be used to set the
  sleep duration between fetch try outs, by default 0.001. Default:
  `"cran"`

## Value

`difftime`, number of days package version was the newest one.

## Note

Results are cached for 30 minutes with `memoise` package. The `crandb` R
packages database is a part of `METACRAN` project, source: Csárdi G,
Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages.
`https://github.com/r-hub/pkgsearch`,
`https://r-hub.github.io/pkgsearch/`. For `source = "cran"`the function
will scrap two CRAN URLS. Works only with CRAN packages. Please as a
courtesy to the R CRAN, don't overload their servers by constantly using
this function.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_lifeduration("memoise")
pacs::pac_lifeduration("memoise", source = "cran")
pacs::pac_lifeduration("dplyr", version = "0.8.0")
pacs::pac_lifeduration("dplyr", at = as.Date("2019-02-14"))
# For Bioconductor packages it will work only for the newest per R version and installed ones.
pacs::pac_lifeduration("S4Vectors")
} # }
```
