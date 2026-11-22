# Getting the most recent package version

using
[`utils::available.packages`](https://rdrr.io/r/utils/available.packages.html)
to get the newest package version.

## Usage

``` r
pac_last(pac, repos = biocran_repos())
```

## Arguments

- pac:

  `character` a package name.

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`character` most recent package version.

## Note

Results are cached for 30 minutes with `memoise` package. For
Bioconductor the newest one per R version.

## Examples

``` r
if (FALSE) { # \dontrun{
pac_last("dplyr")
pac_last("S4Vectors")
} # }
```
