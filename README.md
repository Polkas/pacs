# pacs <a href='https://github.com/polkas/pacs'><img src='man/figures/pacs_logo.png' align="right" width="200px" /></a>
[![R build status](https://github.com/polkas/pacs/workflows/R-CMD-check/badge.svg)](https://github.com/polkas/pacs/actions)
[![CRAN](https://www.r-pkg.org/badges/version/pacs)](https://cran.r-project.org/package=pacs)
[![codecov](https://codecov.io/gh/Polkas/pacs/branch/master/graph/badge.svg)](https://app.codecov.io/gh/Polkas/pacs)


[**For more information please visit the pacs website**](https://polkas.github.io/pacs/).

[**Functions Reference**](https://polkas.github.io/pacs/reference/index.html)

A set of tools that make life easier for developers and maintainers of R packages.

- Validating the library, packages and `renv` lock files.
- Exploring complexity of a certain package like evaluating its size in bytes with dependencies.
- The shiny app complexity could be explored too.
- Assessing life duration of a specific package version.
- Checking a CRAN package check page status for any errors and warnings.
- Retrieving a DESCRIPTION or NAMESPACE file for any package version.
- Comparing DESCRIPTION or NAMESPACE files between different package versions.
- Getting a list of all releases for a specific package.
- The Bioconductor is partly supported.

**An Internet connection is required to take full advantage of most of the features.**

**Almost all calls which requiring an Internet connection are cached (for 30 minutes) by the `memoise` package, so the second invocation of the same command (and arguments) is immediate. Restart the R session if you want to clear cached data.**

## Installation

Development version:

```r
# install.packages("remotes")
remotes::install_github("polkas/pacs")
```

CRAN:

```r
install.packages("pacs")
```
