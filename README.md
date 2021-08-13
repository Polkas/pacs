# pacs
[![R build status](https://github.com/polkas/pacs/workflows/R-CMD-check/badge.svg)](https://github.com/polkas/pacs/actions)
[![CRAN](https://www.r-pkg.org/badges/version/pacs)](https://cran.r-project.org/package=pacs)
[![codecov](https://codecov.io/gh/Polkas/pacs/branch/master/graph/badge.svg)](https://codecov.io/gh/Polkas/pacs)

[Supplementary Tools for R Packages Developers](https://polkas.github.io/pacs/index.html)

- Supplementary utils for CRAN maintainers and R packages developers.
- Validating the library or packages.
- Exploring complexity of a certain package like evaluating sizes in bytes of all its dependencies.
- Assessing life duration of a specific package version.
- Checking a package CRAN check page status for any errors and warnings.
- Retrieving a DESCRIPTION or NAMESPACE file for any package version.
- Comparing DESCRIPTION or NAMESPACE files between different package versions.
- Getting a list of all releases for a specific package.
- The Bioconductor is partly supported.

| Function                            | Description                                                 | 
|:------------------------------------|:-----------------------------------------------|
|`lib_validate`                       | Validate the local library          |
|`pac_validate`             | Validate a specific local package              |
|`pac_deps`               |  CRAN package dependencies with installed or expected versions |
|`pac_deps_timemachine`|  CRAN package dependencies for certain version or time point|
|`pac_description` | CRAN package DESCRIPTION file at Date or for a certain version      |
|`pac_namespace` | CRAN package NAMESPACE file at Date or for a certain version      |
|`pac_lifeduration` | Package version life duration  |
|`pac_health`           | CRAN package version health    |
|`pac_size`             | Size of the package                                       | 
|`pac_timemachine` | CRAN package versions at a specific Date or a Date interval   |
|`pac_compare_versions`               | Compare dependencies between different versions of a CRAN package          |
|`pac_compare_namespace`               | Compare NAMESPACE fields between different versions of a CRAN package  |
|`pac_true_size`                      | True size of the package (with dependencies)| 
|`pacs_base`                          | R base packages                               |
|`pac_last`|The most recent package version|
|`pac_islast`| Checking if a package vecion is the most recent one|
|`pac_on`| Checking if a package is currently inside provided repositories|
|`pac_checkred` | Checking the R CRAN package check page status for any errors and warnings|
|`checked_packages`| Downloading all R CRAN packages check page statuses|
|`pac_checkpage` |   Retrieving the package R CRAN check page|
|`cran_flavors`|  Retrieving all R CRAN servers flavors|
|`biocran_repos()`| Display current Bioconductor and CRAN repositories|

**Hint1**: `Version` variable is mostly a minimal required i.e. max(version1, version2 , ...)

**Hint2**: When working with many packages it is recommended to use global functions, which retrieving data for many packages at once. An example will be usage of `pacs::checked_packages()` over `pacs::pac_checkpage` (or `pacs::pac_checkred`). Another example will be usage of `utils::available.packages` over `pacs::pac_last`. Finally most important one will be `pacs::lib_validate` over `pacs::pac_validate` and `pacs::pac_checkred` and others.

**Hint3**: Almost all time consuming calculations are cached (for 1 hour) with `memoise::memoise` package, second invoke of the same call is instantaneous.

**Hint4**: Use `parallel::mclapply` (Linux and Mac) or `parallel::parLapply` (Windows, Linux and Mac) to speed up loop calculations. Nevertheless under `parallel::mclapply` computation results are NOT cached with `memoise` package. Warning: Parallel computations might be unstable.

## Installation

```r
# install.packages("remotes")
remotes::install_github("polkas/pacs")
```

## Validate the library

This procedure will be crucial for R developers as clearly showing the possible broken packages inside the local library. Thus we could assess which packages require versions update.

Default validation of the library.

```r
pacs::lib_validate()
```

The full library validation require activation of two additional arguments `lifeduration` and `checkred`. Additional arguments are on default turned off as are time consuming, for `lifeduration` assessment might take even few minutes for bigger libraries.

Assessment of status on CRAN check pages takes only few additional seconds even for all R CRAN packages. `pacs::checked_packages()` is used to gather all packages check statuses for all CRAN servers.

```r
pacs::lib_validate(checkred = list(scope = c("ERROR", "FAIL")))
```

When `lifeduration` is triggered then assessment might takes even few minutes.

```r
pacs::lib_validate(lifeduration = TRUE, 
                   checkred = list(scope = c("ERROR", "FAIL")))
```

Not only `scope` field inside `checkred` list could be updated, to remind any of `c("ERROR", "FAIL", "WARN", "NOTE")`. We could specify `flavors` field inside the  `checkred` list argument and narrow the tested machines. The full list of CRAN servers (flavors) might be get with `pacs::cran_flavors()$Flavor`.

```r
pacs::lib_validate(checkred = list(scope = c("ERROR", "FAIL"), 
                   flavors = pacs::cran_flavors()$Flavor[1:2]))
```

## R CRAN packages check page statuses

`checked_packages` was built to extend the `.packages` family functions, like `utils::installed.packages()` and `utils::available.packages()`. 
`pacs::checked_packages` retrieves all current packages checks from [CRAN webpage](https://cran.r-project.org/web/checks/check_summary_by_package.html).

```r
pacs::checked_packages()
```

Use `pacs::pac_checkpage("dplyr")` to get per package check page. However `pacs::checked_packages()` will be more efficient for many packages. Remember that `pacs::checked_packages()` result is cached after the first invoke.

## Time machine - Package version at Date or specific Date interval

Using R CRAN website to get packages version/versions used at a specific Date or a Date interval.

```r
pacs::pac_timemachine("dplyr")
pacs::pac_timemachine("dplyr", version = "0.8.0")
pacs::pac_timemachine("dplyr", at = as.Date("2017-02-02"))
pacs::pac_timemachine("dplyr", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))
pacs::pac_timemachine("dplyr", at = Sys.Date())
```

## Package health

We could find out if a certain package version was lived more than 14 days (or other x limit days). 
If not then we might assume something wrong was with it, as had to be quickly updated.

e.g. `dplyr` under the "0.8.0" version seems to be a broken release, we could find out that it was published only for 1 day.

```r
pacs::pac_lifeduration("dplyr", "0.8.0")
```

With 14 day limit we get a proper health status. We are sure about this state as this is not the newest release. For newest packages we are checking if there are any red messages on CRAN check pages too, specify with a `scope` argument.

```r
pacs::pac_health("dplyr", version = "0.8.0", limit = 14)
```

For the newest package we will check the CRAN check page too, the scope might be adjusted.

```r
pacs::pac_health("dplyr", limit = 14, scope = c("ERROR", "FAIL", "WARN"))
pacs::pac_health("dplyr", limit = 14, scope = c("ERROR", "FAIL", "WARN"), 
                 flavors = pacs::cran_flavors()$Flavor[1])
```

## Package DESCRIPTION file

Reading raw `dcf` DESCRIPTION files scrapped from the github CRAN mirror or if not worked from the CRAN website. 

```r
pacs::pac_description("dplyr")
pacs::pac_description("dplyr", version = "0.8.0")
pacs::pac_description("dplyr", at = as.Date("2019-01-01"))
```

## Package NAMESPACE file

Reading raw NAMESPACE files scrapped from the github CRAN mirror or if not worked from the CRAN website. 

```r
pacs::pac_namespace("dplyr")
pacs::pac_namespace("dplyr", version = "0.8.0")
pacs::pac_namespace("dplyr", at = as.Date("2019-01-01"))
```

## Dependencies for specific version

For the newest release.

```r
pacs::pac_deps("devtools", local = FALSE)$Package
```

For a certain version, might take some time.

```r
pacs::pac_deps_timemachine("dplyr", version = "0.8.1")
```

## Package dependencies and differences between versions

One of the main functionality is to get versions for all package dependencies. 
Versions might come form installed packages or DESCRIPTION files.

`pac_deps` for an extremely fast retrieving of package dependencies, 
packages versions might come from installed ones or from DESCRIPTION files (required minimum).

```r
# Providing more than tools::package_dependencies and packrat:::recursivePackageVersion
# pacs::package_deps is providing the min required version for each package
# Use it to answer what we should have
res <- pacs::pac_deps("shiny", description_v = TRUE)
res
attributes(res)
```

Packages dependencies with versions from DESCRIPTION files.

```r
pacs::pac_deps("shiny", description_v = TRUE)
```

Remote (newest CRAN) package dependencies with versions.

```r
pacs::pac_deps("shiny", local = FALSE)
```

Raw dependencies from DESCRIPTION file

```r
pacs::pac_deps("memoise", description_v = TRUE, recursive = FALSE)
```

Useful functions to get list of base packages. 
You might want to exclude them from final results.

```r
pacs::pacs_base()
# start up loaded, base packages
pacs::pacs_base(startup = TRUE)
```

## Comparing DESCRIPTION or NAMESPACE files between different package versions

Comparing DESCRIPTION file dependencies between local and newest package.
We will get duplicated columns if the local version is the newest one.

```r
pacs::pac_compare_versions("shiny")
```

Comparing DESCRIPTION file dependencies between package versions.

```r
pacs::pac_compare_versions("shiny", "1.4.0", "1.5.0")

pacs::pac_compare_versions("shiny", "1.4.0", "1.6.0")
# to newest release
pacs::pac_compare_versions("shiny", "1.4.0")
```

Comparing NAMESPACE between local and newest package.

```r
pacs::pac_compare_namespace("shiny")
```

Comparing NAMESPACE between package versions.

```r
pacs::pac_compare_namespace("shiny", "1.0.0", "1.5.0")
# e.g. only exports 
pacs::pac_compare_namespace("shiny", "1.0.0", "1.5.0")$exports

# to newest release
pacs::pac_compare_namespace("shiny", "1.0.0")
```

## Package Weight Case Study: `devtools`

Take into account that packages sizes are appropriate for your local system (`Sys.info()`).
Installation with `install.packages` and some `devtools` functions might result in different packages sizes.

```r
# if not have
install.packages("devtools")
install.packages("shiny")
```

Size of the `devtools` package:

```r
cat(pacs::pac_size("devtools") / 10**6, "MB", "\n")
```

True size of the package as taking into account its dependencies.
At the time of writing it, it is `113MB` for `devtools` without base packages (`Mac OS arm64`).

```r
cat(pacs::pac_true_size("devtools") / 10**6, "MB", "\n")
```

A reasonable assumption might be to count only dependencies which are not used by any other package.
Then we could use `exclude_joint` argument to limit them.
However hard to assume if your local installation is a reasonable proxy for an average user.

```
# exclude packages if at least one other package use it too
cat(pacs::pac_true_size("devtools", exclude_joint = 1L) / 10**6, "MB", "\n")
```

Might be useful to check the number of dependencies too:

```r
pacs::pac_deps("devtools", local = TRUE)$Package
```

## packages versions

Small guide how to work with packages versions ("package_version" and "numeric_version" classes).

```r
# if you do not remember a calss name use class(packageVersion("base"))
v1 <- `class<-`(list(c(1,1,1)), c("package_version", "numeric_version"))
v2 <- `class<-`(list(c(1,0,0)), c("package_version", "numeric_version"))
v1 > v2
str(v1)
# comparing with utils::compareVersion
compareVersion("1.1.1", "1.0.0")
# comparing versions vector with pacs::compareVersionsMax
pacs::compareVersionsMax(c("1.1.1", "1.0.0", "3.3.3"))
```
