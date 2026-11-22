# CRAN package health state at a specific Date or for a specific version

a package health for a certain version or at a specific Date. By default
works for the newest package version. A healthy package was published
for more than x days, where default is 14 days. CRAN team gives around
one/two week to resolved a package which gave errors under the check
page. The newest release is checked for any warnings/errors on the R
CRAN package check page.

## Usage

``` r
pac_health(
  pac,
  version = NULL,
  at = NULL,
  limit = 14,
  scope = c("ERROR", "FAIL"),
  flavors = NULL,
  lib.loc = .libPaths(),
  repos = "https://cran.rstudio.com/",
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

- limit:

  `numeric` at least days to treat as healthy, "\>=limit". Default: 14

- scope:

  `character` vector scope of the check, accepted values c("ERROR",
  "FAIL", "WARN", "NOTE"). Default: `c("ERROR", "FAIL")`

- flavors:

  `character` vector of CRAN server names to consider, possible names
  could be get with `pacs::cran_flavors()$Flavor`. The
  [`pacs::match_flavors()`](https://polkas.github.io/pacs/reference/match_flavors.md)
  function could be used to get CRAN server names matched for your local
  `OS`. By default all CRAN machines are considered `NULL` value.
  Default: `NULL`

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- repos:

  character vector repositories URLs to use. Default
  `https://cran.rstudio.com/`

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

`logical` if a package is healthy, or `NA` when the required release or
CRAN check information is unavailable.

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
pacs::pac_health("memoise")
pacs::pac_health("dplyr", version = "0.8.0", limit = 14)
pacs::pac_health("dplyr", at = as.Date("2019-02-14"))
pacs::pac_health("dplyr", limit = 14, scope = c("ERROR", "FAIL"))
} # }
```
