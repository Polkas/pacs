# Packages life duration for a specific version

packages life duration for certain versions.

## Usage

``` r
pacs_lifeduration(
  pacs,
  versions,
  source = c("crandb", "loop_crandb", "loop_cran"),
  lib.loc = .libPaths(),
  repos = biocran_repos()
)
```

## Arguments

- pacs:

  `character` vector of packages names.

- versions:

  `character` vector of packages versions.

- source:

  character one of `c("crandb", "loop_crandb", "loop_cran")`. The
  `"crandb"` works if less than `getOption("pacs.crandb_limit")`
  (currently 500) packages are looked for. When `crandb` linked source
  is used then `options(pacs.crandb_ntry = 3)` and
  `options(pacs.crandb_nsleep = 0.001)` can be used to control the
  fetch. `"pacs.crandb_ntry"` can be used to set the maximum number of
  try outs, by default 3. `"pacs.crandb_nsleep"` can be used to set the
  sleep duration between fetch try outs, by default 0.001. Default:
  `"crandb"`

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`data.frame` with two columns package name and life duration.

## Note

Results are cached for 30 minutes with `memoise` package. The `crandb` R
packages database is a part of `METACRAN` project, source: Csárdi G,
Salmon M (2022). `pkgsearch`: Search and Query CRAN R Packages.
`https://github.com/r-hub/pkgsearch`,
`https://r-hub.github.io/pkgsearch/`. For `source = "loop_cran"`the
function will scrap two CRAN URLS. Works only with CRAN packages. Please
as a courtesy to the R CRAN, don't overload their servers by constantly
using this function.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pacs_lifeduration(c("dplyr", "tidyr"), c("1.0.0", "1.2.0"))
pacs::pacs_lifeduration(c("dplyr", "tidyr"), c("1.0.0", "1.2.0"), source = "loop_cran")
# last versions
pacs::pacs_lifeduration(c("dplyr", "tidyr"), sapply(c("dplyr", "tidyr"), pacs::pac_last))
} # }
```
