# Retrieving all Bioconductor releases

Retrieving all Bioconductor releases. The data is downloaded from
Bioconductor's machine-readable `https://bioconductor.org/config.yaml`.

## Usage

``` r
bio_releases()
```

## Value

`data.frame` with `Release`, `Date`, `Software packages`, and `R`
columns, or `NA` if the configuration cannot be read. The configuration
does not provide historical software-package counts, so
`Software packages` is `NA`. The `R` column is `NA` for releases without
an R version mapping.

## Note

Only versions up to the current Bioconductor release with a release date
are included, newest first. Dates and R versions follow the Bioconductor
configuration and may differ from other historical sources. Successful
results are cached for 30 minutes with the `memoise` package; failures
are not cached.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::bio_releases()
} # }
```
