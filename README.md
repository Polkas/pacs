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
|:------------------------------------|:------------------------------------------------------------|
|`lib_validate`                       | Validating the library                                      |
|`pac_validate`/`pacs_validate`       | Package/s: Validation              |
|`pac_deps`/`pacs_deps`               |  Package/s dependencies with installed or expected versions |
|`pac_deps_timemachine`/`pacs_deps_timemachine`|  Package/s dependencies for certain version or time point|
|`pac_description`/`pacs_description` | Package/s description at Date or for a certain version      |
|`pac_lifeduration`/`pacs_lifeduration` | Package/s version/s life duration  |
|`pac_health`/`pacs_health`           | Package/s health    |
|`pac_size`/`pacs_size`               | Size of the package/s                                       | 
|`pac_timemachine`/`pacs_timemachine` | Package/s version/s at a specific Date or a Date interval   |
|`pac_compare_versions`               | Compare dependencies of specific package versions           |
|`pac_true_size`                      | True size of the package (with dependencies too)            | 
|`pacs_base`                          | R base packages                                             |
|`pac_checkred`/`pacs_checkred`                          |   Checking a package CRAN check page status for any errors and warnings |

Hint: `Version` variable is mostly a minimal required i.e. max(version1, version2 , ...)

Hint2: all time consuming calculations are cached (for 1 hour) with `memoise` package, second invoke of the same call is instantaneous.

## Validate the library

This procedure will be crucial for R developers as clearly showing the possible broken packages inside a library. Thus we could assess which packages require versions update. Sometimes we might even 

Default validation of library.

```r
pacs::lib_validate()
```

The full library validation require activation of two additional arguments `lifeduration` and `checkred`. Additional arguments are on default turned off as are time consuming, assessment might take even few minutes.

```r
pacs::lib_validate(lifeduration = TRUE, checkred = TRUE)
```

### Package Weight Case Study: `devtools`

Take into account that the size is appropriate for you system `Sys.info()`.
Installation with `install.packages` and some `devtools` functions might result in different package sizes.

```r
# if not have
install.packages("devtools")
install.packages("shiny")
```

Size of a package:

```r
cat(pacs::pac_size("devtools") / 10**6, "Mb", "\n")
```

True size of a package as taking into account its dependencies.
At the time of writing it, it is `113Mb` for `devtools` without base packages (Mac OS arm64).

```r
cat(pacs::pac_true_size("devtools") / 10**6, "Mb", "\n")
# cat(pacs::pac_true_size("devtools") / 10**6, "Mb", "\n")
```

A reasonable assumption might be to count only dependencies which are not used by any other package.
Then we could use `exclude_joint` argument to limit them.
However hard to assume if your local installation is a reasonable proxy for average user.

```
# exclude packages if at least one other package use it too
cat(pacs::pac_true_size("devtools", exclude_joint = 1L) / 10**6, "Mb", "\n")
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
pac_timemachine("dplyr", at = as.Date("2017-02-02"))
pac_timemachine("dplyr", from = as.Date("2017-02-02"), to = as.Date("2018-04-02"))
pac_timemachine("dplyr", at = Sys.Date())
```

```r
pacs_timemachine(c("dplyr", "shiny"), from = as.Date("2018-06-30"), to = as.Date("2019-01-01"))
pacs_timemachine(c("dplyr", "shiny"), at = Sys.Date())
```

CRAN packages Date mirror - will take some time (even few minutes):

```r
pacs_timemachine(rownames(installed.packages()), at = as.Date("2020-08-08"))
```

## Package health

We could find out if a certain package version was live more than 14 days (or other updated). 
If not then we might assume something wrong was with it, as had to be quickly updated.

e.g. `dplyr` under the "0.8.0" version seems to be a broken release, we could find out that it was published only for 1 day.

```r
pac_lifeduration("dplyr", "0.8.0")
```

With 14 day limit we get a proper health status. We are sure about this state as this is not the newest release. For newest packages (released less than x days) we are checking if there are any red messages on CRAN check pages.

```r
pac_health("dplyr", version = "0.8.0", limit = 14)
```

All packages health, skip non CRAN packages - will take some time (even few minutes):

```r
all_pacs_health <- pacs_health(rownames(installed.packages()))
```

## Package DESCRIPTION file

Reading a raw `dcf` file DESCRIPTION files scrapped from CRAN website. 

```r
pac_description("dplyr", version = "0.8.0")
pac_description("dplyr", at = as.Date("2019-01-01"))
```

For many packages:

```r
pacs_description(c("dplyr", "shiny"), version = c("0.8.0", "1.5.0"))
pacs_description(c("dplyr", "shiny"), at = as.Date("2019-01-01"))
```

## Dependencies for version and remote one

For newest release.

```r
pacs::pac_deps("devtools", local = FALSE)$Package
```

For certain version, might take some time.

```r
pacs::pac_deps_timemachine("dplyr", version = "0.8.1")
```

## Package dependencies and differences between versions

The crucial functionality is to get versions for all package dependencies. 
Versions might come form installed packages or all DESCRIPTION files.

```r
install.packages("shiny")
```

Useful functions to get list of base packages. 
You might want to exclude them from final results.

```r
pacs_base()
# start up loaded, base packages
pacs_base(startup = TRUE)
```

`pac_deps` for extremely fast retrieve of package dependencies, 
packages versions might come from installed ones or from description files (required minimum).

```r
# Providing more than tools::package_dependencies and packrat:::recursivePackageVersion
# pacs::package_deps is providing the min required version for each package
# Use it to answer what we should have
res <- pacs::pac_deps("shiny")
res
attributes(res)
```

Packages dependencies with versions from description files.

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

This is for sure something new on the R market.
comparing dependencies per package versions.

```r
# It was used withr::with_temp_libpaths and devtools::install_version for this task

pacs::pac_compare_versions("shiny", "1.4.0", "1.5.0")

pacs::pac_compare_versions("shiny", "1.4.0", "1.6.0")
```

## Packages dependencies

```r
all_deps <- pacs_deps()
```

```r
pacs_deps(c("stats", "shiny"))
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
