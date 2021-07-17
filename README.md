# pacs
[![R build status](https://github.com/polkas/pacs/workflows/R-CMD-check/badge.svg)](https://github.com/polkas/pacs/actions)
[![CRAN](http://www.r-pkg.org/badges/version/pacs)](https://cran.r-project.org/package=pacs)
[![codecov](https://codecov.io/gh/Polkas/pacs/branch/master/graph/badge.svg)](https://codecov.io/gh/Polkas/pacs)

R packages utils

Hint: `Version` variable is mostly a minimal required i.e. max(version1, version2 , ...)

- Checking packages dependencies with their versions. 
- Validating the library for possible wrong packages versions (what we have vs what we should have). 
- Exploring complexity of packages.

| Function                            | Description                                                 | 
|:------------------------------------|:------------------------------------------------------------|
|`pac_deps`/`pacs_deps`               |  Package/s dependencies with installed or expected versions |
|`pac_deps_timemachine`/`pacs_deps_timemachine`|  Package/s dependencies for certain version or time point|
|`pac_description`/`pacs_description` | Package/s description at Date or for a certain version      |
|`pac_validate`/`pacs_validate`       | Package/s: What we have vs What we should have              |
|`pac_health`/`pacs_health`           | Package/s health, if a version was live more than 7 days    |
|`pac_size`/`pacs_size`               | Size of the package/s                                       | 
|`pac_timemachine`/`pacs_timemachine` | Package/s version/s at a specific Date or a Date interval   |
|`pac_compare_versions`               | Compare dependencies of specific package versions           |
|`pac_true_size`                      | True size of the package (with dependencies too)            | 
|`pacs_base`                          | R base packages                                             |
|`lib_validate`                       | Library: What we have vs What we should have                |


### Package Weight Case Study: devtools

Take into account that the size is appropriate for you system `Sys.info()`.
Installation with `install.packages` and some `devtools` functions might result in different package sizes.

```r
install.packages("devtools")
```

Size of a package:

```r
cat(pacs::pac_size("devtools") / 10**6, "Mb", "\n")
```

True size of a package as taking into account its dependencies.
At the time of writing it, it is `113Mb` for `devtools` without base packages (Mac OS arm64).

```r
cat(pacs::pac_true_size("devtools") / 10**6, "Mb", "\n")
# cat(pacs::pac_true_size("devtools", base = TRUE) / 10**6, "Mb", "\n")
```

A reasonable assumption might be to count only dependencies which are not used by any other package.
Then we could use `exclude_joint` argument to limit them.
However hard to assume if your local installation is a reasonable proxy for avarage user.

```
# exclude packages if at least one other package use it too
cat(pacs::pac_true_size("devtools", exclude_joint = 1L) / 10**6, "Mb", "\n")
```

Might be useful to check the number of dependencies too:

```r
pacs::pac_deps("devtools")$Package
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

We could find out if a certain package version was live more than 7 days. 
If not then we might assume sth wrong was with it, as had to be quickly updated.

e.g. `dplyr` under the "0.8.0" version seems to be a broken release, with `pac_timemachine("dplyr")` we could find out that it was published only for 1 day.

```r
pac_health("dplyr", version = "0.8.0")
```

All packages health, skip non CRAN packages - will take some time (even few minutes):

```r
all_pacs_health <- pacs_health(rownames(installed.packages()))
#not healthy packages
Filter(function(x) isFALSE(x) && (class(x) == "sure"), 
       all_pacs_health)
```

## Package DESCRIPTION file

Reading a raw dcf DESCRIPTION files scrapped from CRAN website. 

```r
pac_description("dplyr", version = "0.8.0")
pac_description("dplyr", at = as.Date("2019-01-01"))
```

For many packages:

```r
pacs_description(c("dplyr", "shiny"), version = c("0.8.0", "1.5.0"))
pacs_description(c("dplyr", "shiny"), at = as.Date("2019-01-01"))
```

## Install version

A true installation of certain package version, as the dependencies are taken for its release date.

```r
# will fail!!!!
# this version was released for less than 7 days, so it is assumed to be unhealthy (`pac_health`).
pac_install_version("dplyr", "0.8.0")
```

```r
pac_install_version("dplyr", "0.8.1")
```

## Package dependencies and diffeneces between versions

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
packages versions might come from installed ones or from desciption files (required minimum).

```r
# Providing more than tools::package_dependencies and packrat:::recursivePackageVersion
# pacs::package_deps is providing the min required version for each package
# Use it to answer what we should have
res <- pacs::pac_deps("shiny")
res
attributes(res)
```

Packages depndencies with versions from description files.

```r
pacs::pac_deps("shiny", description_v = TRUE)
```

Remote (newest CRAN) package depndencies with versions.

```r
pacs::pac_deps("shiny", remote = TRUE)
```

Sth new on the R market.
comparing dependencies per package versions.

```r
# It was used withr::with_temp_libpaths and devtools::install_version for this task

pacs::pac_compare_versions("shiny", "1.4.0", "1.5.0")

pacs::pac_compare_versions("shiny", "1.4.0", "1.6.0")
```

## Dependencies taking into account newest r cran packages

`pacrat` solution over `available.packages` which access remote repository.

`ranger` is not installed locally.

```r
packrat:::recursivePackageDependencies("ranger", lib.loc = NULL)
```

## Packages dependencies

```r
all_deps <- pacs_deps()
```

```r
pacs_deps(c("stats", "shiny"))
```

## What we have vs What we should have 

Using Description files to check what we should have "at least".

Package:

```r
pac_validate("devtools")
# Packages
# pacs_validate(c("devtools", "pacs"))
```

Whole library:

```r
# Test with adding older cachem than extected
withr::with_temp_libpaths({
devtools::install_version("shiny", "1.4.0-2")
devtools::install_version("rlang", "0.4.6")
pacs::lib_validate()
})
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
