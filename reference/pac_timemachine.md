# Package metadata for all releases

Using CRAN website to get a package metadata used at a specific Date or
a Date interval or for specific version.

## Usage

``` r
pac_timemachine(
  pac,
  at = NULL,
  from = NULL,
  to = NULL,
  version = NULL,
  source = c("cran", "crandb")
)
```

## Arguments

- pac:

  `character` a package name.

- at:

  `Date` from which to take the version. Default: `NULL`

- from:

  `Date` the lower limit. Default: `NULL`

- to:

  `Date` the upper limit. Default: `NULL`

- version:

  `character` version of a package. Default: `NULL`

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

`data.frame` with 7 columns

- Package:

  character package name.

- Version:

  character package version.

- Released:

  character release Date

- Archived:

  character archived Date.

- LifeDuration:

  `difftime` number of days the version was the newest one.

- URL:

  character the suffix of the base URL to `tar.gz` file. The base part
  of URL in the result is `https://cran.r-project.org/src/contrib/`.

- Size:

  character size of the `tar.gz` file.

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
pacs::pac_timemachine("dplyr")
pacs::pac_timemachine("dplyr", at = as.Date("2017-02-02"))
pacs::pac_timemachine("dplyr", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))
pacs::pac_timemachine("dplyr", at = Sys.Date())
pacs::pac_timemachine("tidyr", from = as.Date("2020-06-01"), to = Sys.Date())
} # }
```
