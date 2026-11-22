# Read a file from a GitHub CRAN repository

Read a file from a GitHub CRAN repository.

## Usage

``` r
read_github_file(pac, version, file, repos = "https://cran.rstudio.com/")
```

## Arguments

- pac:

  `character` a package name.

- version:

  `character` version of a package. Default: `NULL`

- file:

  `character` file name to read. Possible values are `DESCRIPTION`,
  `NEWS` and `NAMESPACE`.

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

## Note

if the file is not found in the GitHub repository, it will try to find
it in the CRAN archive.
