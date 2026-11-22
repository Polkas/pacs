# Checking if a package version is the most recent one

checking if a package version is the most recent one, by default the
installed version is compared.

## Usage

``` r
pac_islast(pac, version = NULL, lib.loc = .libPaths(), repos = biocran_repos())
```

## Arguments

- pac:

  `character` a package name.

- version:

  `character` version of a package. Default: `NULL`

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Value

`logical` if a package is inside repositories.

## Note

Results are cached for 30 minutes with `memoise` package.

## Examples

``` r
if (FALSE) { # \dontrun{
pac_islast("memoise")
pac_islast("dplyr", version = "1.0.0")
pac_islast("S4Vectors")
pac_islast("S4Vectors", version = pac_last("S4Vectors"))
} # }
```
