# pacs
[![R build status](https://github.com/polkas/pacs/workflows/R-CMD-check/badge.svg)](https://github.com/polkas/pacs/actions)
[![CRAN](https://www.r-pkg.org/badges/version/pacs)](https://cran.r-project.org/package=pacs)
[![codecov](https://codecov.io/gh/Polkas/pacs/branch/master/graph/badge.svg)](https://app.codecov.io/gh/Polkas/pacs)

[Supplementary Tools for R Packages Developers](https://polkas.github.io/pacs/index.html)

**For more information please visit the package vignette: [Getting Started](https://polkas.github.io/pacs/articles/GettingStarted.html).**

- Validating the library or packages.
- Exploring complexity of a certain package like evaluating sizes in bytes of all its dependencies.
- Assessing life duration of a specific package version.
- Checking a CRAN package check page status for any errors and warnings.
- Retrieving a DESCRIPTION or NAMESPACE file for any package version.
- Comparing DESCRIPTION or NAMESPACE files between different package versions.
- Getting a list of all releases for a specific package.
- The Bioconductor is partly supported.

## Installation

```r
# install.packages("remotes")
remotes::install_github("polkas/pacs")
```

or from CRAN:

```r
install.packages("pacs")
```

## Functions Reference

[Functions Reference](https://polkas.github.io/pacs/reference/index.html)

| Function                            | Description                                                 | 
|:------------------------------------|:-----------------------------------------------|
|`lib_validate`                       | Validate the local library          |
|`pac_validate`             | Validate a specific local package              |
|`pac_deps`               |  R CRAN package dependencies with installed or expected versions |
|`pac_deps_timemachine`|  R CRAN package dependencies for certain version or time point|
|`pac_description` | R CRAN package DESCRIPTION file at Date or for a certain version      |
|`pac_namespace` | R CRAN package NAMESPACE file at Date or for a certain version      |
|`pac_lifeduration` | Package version life duration  |
|`pac_health`           | R CRAN package version health    |
|`pac_size`             | Size of the package                                       | 
|`pac_timemachine` | R CRAN package versions at a specific Date or a Date interval   |
|`pac_compare_versions` | Compare dependencies between different versions of a R CRAN package          |
|`pac_compare_namespace`| Compare NAMESPACE fields between different versions of a R CRAN package  |
|`pac_true_size`    | True size of the package (with dependencies)| 
|`pacs_base`        | R base packages                               |
|`pac_last`| The most recent package version|
|`pac_islast`| Checking if a package version is the most recent one|
|`pac_isin`| Checking if a package is currently inside provided repositories|
|`pac_checkred` | Checking the R CRAN package check page status for any errors and warnings|
|`pac_checkpage` | Retrieving the R CRAN package check page|
|`checked_packages`| Retrieving all R CRAN packages check page statuses|
|`cran_flavors`|  Retrieving all R CRAN servers flavors|
|`biocran_repos`| Display current Bioconductor and CRAN repositories|
|`bio_releases`| Retrieving all Bioconductor releases|
