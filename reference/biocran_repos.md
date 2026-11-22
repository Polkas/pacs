# CRAN and Bioconductor repositories

CRAN and Bioconductor repositories. The newest Bioconductor release for
the specific R version is assumed.

## Usage

``` r
biocran_repos(version = NULL)
```

## Arguments

- version:

  `character` the Bioconductor release. By default the newest
  Bioconductor release for the specific R version is assumed, if not
  available only CRAN repository is returned. Available Bioconductor
  versions for your R version could be checked with
  [`pacs::bio_releases()`](https://polkas.github.io/pacs/reference/bio_releases.md).
  Default NULL

## Value

`named character` vector of repositories.

## Note

The Internet connection is needed to get Bioconductor repositories. If
the release configuration is unavailable or no release matches, a
message is issued and only the CRAN repository is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::biocran_repos()
} # }
```
