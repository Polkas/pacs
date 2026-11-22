# Retrieving all R CRAN packages check pages statuses.

Retrieving all R CRAN packages check pages statuses. The data is
downloaded from
`https://cran.r-project.org/web/checks/check_summary_by_package.html`.

## Usage

``` r
checked_packages()
```

## Value

`data.frame` with the same structure as the html table on
`https://cran.r-project.org/web/checks/check_summary_by_package.html`.

## Note

Results are cached for 30 minutes with `memoise` package. Some packages
could be duplicated as not all tests are performed for a new version so
two versions still coexists. Checks with asterisks (\*) indicate that
checking was not fully performed, this is a case for less than 1% of all
packages.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::checked_packages()
} # }
```
