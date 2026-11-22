# Checking if a package is in repositories

using
[`utils::available.packages`](https://rdrr.io/r/utils/available.packages.html)
to check if package is in repositories.

## Usage

``` r
pac_isin(pac, repos = biocran_repos())
```

## Arguments

- pac:

  `character` a package name.

- repos:

  character vector base URLs of the repositories to use. By default
  checking CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`logical` if a package is inside repositories.

## Note

Results are cached for 30 minutes with `memoise` package.

## Examples

``` r
if (FALSE) { # \dontrun{
pac_isin("dplyr")
pac_isin("dplyr", repos = "https://cran.rstudio.com/")
pac_isin("dplyr", repos = biocran_repos()[grep("Bio", names(biocran_repos()))])
} # }
```
