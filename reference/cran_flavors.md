# Retrieving all R CRAN servers flavors

Retrieving all R CRAN servers flavors. The data is downloaded from
`https://cran.r-project.org/web/checks/check_flavors.html`.

## Usage

``` r
cran_flavors()
```

## Value

`data.frame` with the same structure as the html table on
`https://cran.r-project.org/web/checks/check_flavors.html`.

## Note

Results are cached for 30 minutes with `memoise` package.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::cran_flavors()
} # }
```
