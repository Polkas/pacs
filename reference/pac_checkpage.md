# Retrieving the R CRAN package check page

Retrieving the R CRAN package check page.

## Usage

``` r
pac_checkpage(pac)
```

## Arguments

- pac:

  `character` a package name.

## Value

`data.frame`.

## Note

Results are cached for 30 minutes with `memoise` package. If you need to
check many packages at once then is recommended usage of
[`pacs::checked_packages`](https://polkas.github.io/pacs/reference/checked_packages.md).
Please as a courtesy to the R CRAN, don't overload their server by
constantly using this function.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_checkpage("dplyr")
} # }
```
