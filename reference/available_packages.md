# List Available Packages at CRAN-like Repositories

available_packages returns a matrix of details corresponding to packages
currently available at one or more repositories. The current list of
packages is downloaded over the internet (or copied from a local
mirror).

## Usage

``` r
available_packages(repos)
```

## Arguments

- repos:

  character vector URLs of the repositories to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)
