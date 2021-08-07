# pacs
[![R build status](https://github.com/polkas/pacs/workflows/R-CMD-check/badge.svg)](https://github.com/polkas/pacs/actions)
[![CRAN](https://www.r-pkg.org/badges/version/pacs)](https://cran.r-project.org/package=pacs)
[![codecov](https://codecov.io/gh/Polkas/pacs/branch/master/graph/badge.svg)](https://codecov.io/gh/Polkas/pacs)

[Supplementary Tools for R Packages Developers](https://polkas.github.io/pacs/index.html)

- Supplementary utils for CRAN maintainers and R packages developers.
- Validating the library or packages health condition.
- Exploring complexity of a certain package like evaluating sizes in bytes of all its dependencies.
- Assessing life duration of a specific package version.
- Checking a package CRAN check page status for any errors and warnings.
- Retrieving a DESCRIPTION file for any package version. 
- Getting a list of all releases for a specific package.

| Function                            | Description                                                 | 
|:------------------------------------|:--------------------------------------------------|
|`lib_validate`                       | Validate the local library          |
|`pac_validate`             | Validate a specific local package              |
|`pac_deps`               |  Package dependencies with installed or expected versions |
|`pac_deps_timemachine`|  Package dependencies for certain version or time point|
|`pac_description` | Package DESCRIPTION file at Date or for a certain version      |
|`pac_namespace` | Package NAMESPACE file at Date or for a certain version      |
|`pac_lifeduration` | Package version life duration  |
|`pac_health`           | Package version health    |
|`pac_size`             | Size of the package                                       | 
|`pac_timemachine` | Package versions at a specific Date or a Date interval   |
|`pac_compare_versions`               | Compare dependencies between different versions of a package          |
|`pac_compare_exports`               | Compare NAMESPACE exports between different versions of a package          |
|`pac_true_size`                      | True size of the package (with dependencies)| 
|`pacs_base`                          | R base packages                               |
|`pac_last`|  The most recent package version|
|`pac_checkred` | Checking the R CRAN package check page status for any errors and warnings|

**Hint**: `Version` variable is mostly a minimal required i.e. max(version1, version2 , ...)

**Hint2**: Almost all time consuming calculations are cached (for 1 hour) with `memoise::memoise` package, second invoke of the same call is instantaneous. For `pacs::lib_validate` when `parallel::mclapply` is used results are not cached.

**Hint3**: Use `parallel::mclapply` (Linux and Mac) or `parallel::parLapply` (Windows, Linux and Mac) to speed up calculations. Nevertheless under `parallel::mclapply` computation results are NOT cached with `memoise` package.

**Hint4**: When your library have more than a few thousand packages (`nrow(utils::installed.packages())`), please be patient when running Internet based functions. Optionally the third hint could be applied, so usage of parallel computation. Ps. Now, the whole R CRAN library contains around 18 thousands packages.

## Validate the library

This procedure will be crucial for R developers as clearly showing the possible broken packages inside a library. Thus we could assess which packages require versions update.

Default validation of the library.

```r
pacs::lib_validate()
```

The full library validation require activation of two additional arguments `lifeduration` and `checkred`. Additional arguments are on default turned off as are time consuming, assessment might take even few minutes.

```r
pacs::lib_validate(lifeduration = TRUE, checkred = c("ERROR", "FAIL"))
```

### Package Weight Case Study: `devtools`

Take into account that packages sizes are appropriate for you local system (`Sys.info()`).
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

## Time machine - Package version at Date or specific Date interval

Using R CRAN website to get packages version/versions used at a specific Date or a Date interval.  
Please as a courtesy to the R CRAN, don't overload their server by constantly using these functions.

```r
pac_timemachine("dplyr")
pac_timemachine("dplyr", version = "0.8.0")
pac_timemachine("dplyr", at = as.Date("2017-02-02"))
pac_timemachine("dplyr", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))
pac_timemachine("dplyr", at = Sys.Date())
```

CRAN packages mirror at Date - will take some time (even few minutes):

```r
all_timemachine <- lapply(rownames(installed.packages()), function(x) pac_timemachine(x, at = as.Date("2020-08-08")))
# parallely for non Windows machines
options(mc.cores = parallel::detectCores() - 1) # once per session
all_timemachine <- mclapply(rownames(installed.packages()), function(x) pac_timemachine(x, at = as.Date("2020-08-08")))
```

## Package health

We could find out if a certain package version was lived more than 14 days (or other x limit days). 
If not then we might assume something wrong was with it, as had to be quickly updated.

e.g. `dplyr` under the "0.8.0" version seems to be a broken release, we could find out that it was published only for 1 day.

```r
pac_lifeduration("dplyr", "0.8.0")
```

With 14 day limit we get a proper health status. We are sure about this state as this is not the newest release. For newest packages (released less than 14 days ago) we are checking if there are any red messages on CRAN check pages.

```r
pac_health("dplyr", version = "0.8.0", limit = 14)
```

All packages health, skip non CRAN packages - will take some time (even few minutes):

```r
all_pacs_health <- lapply(rownames(installed.packages()), function(x) pacs::pacs_health(x))
# parallely for non Windows machines
options(mc.cores = parallel::detectCores() - 1) # once per session
all_pacs_health <- mclapply(rownames(installed.packages()), function(x) pacs::pacs_health(x))
```

## Package DESCRIPTION file

Reading raw `dcf` DESCRIPTION files scrapped from the github CRAN mirror or if not worked from the CRAN website. 

```r
pacs::pac_description("dplyr", version = "0.8.0")
pacs::pac_description("dplyr", at = as.Date("2019-01-01"))
```

## Dependencies for specific version

For the newest release.

```r
pacs::pacs::pac_deps("devtools", local = FALSE)$Package
```

For a certain version, might take some time.

```r
pacs::pac_deps_timemachine("dplyr", version = "0.8.1")
```

## Package dependencies and differences between versions

The crucial functionality is to get versions for all package dependencies. 
Versions might come form installed packages or DESCRIPTION files.

Useful functions to get list of base packages. 
You might want to exclude them from final results.

```r
pacs::pacs_base()
# start up loaded, base packages
pacs::pacs_base(startup = TRUE)
```

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

Comparing NAMESPACE exports between local and newest package.

```r
pacs::pac_compare_exports("shiny")
```

Comparing NAMESPACE exports between package versions.

```r
pacs::pac_compare_exports("shiny", "1.4.0", "1.5.0")

pacs::pac_compare_exports("shiny", "1.4.0", "1.6.0")
# to newest release
pacs::pac_compare_exports("shiny", "1.0.0")
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

## Bonus - base solutions

What we have now:

```r
# installed packages might be misleading as the current installation could be unhealthy
# Installation (utils::installed.packages()) might be easy broken with e.g. devtools::install_version
install_pacs <- utils::installed.packages()
deps_pacs <- tools::package_dependencies("shiny", db = install_pacs, recursive = T, which = c("Depends", "Imports", "LinkingTo"))[[1]]
install_pacs[install_pacs[, c("Package")] %in% deps_pacs, ][,c("Package", "Version")]

# Using R CRAN mirror
avail_pacs <- utils::available.packages()
deps_pacs <- tools::package_dependencies("shiny", db = avail_pacs, recursive = T, which = c("Depends", "Imports", "LinkingTo"))[[1]]
avail_pacs[avail_pacs[, c("Package")] %in% deps_pacs, ][,c("Package", "Version")]
```
 
Dependencies in both directions:

```r
# For certain fields (which) please use at least R 4.1.0
# default on utils::available.packages - remote R mirror
# unfortunately omitting versions
all_dependencies <- tools::package_dependencies(recursive = TRUE, db = installed.packages(),
                                                which = c("Depends", "Imports", "LinkingTo"))
                                                
# in other direction packages depends on a certain package
tools::dependsOnPkgs("stats")
```
