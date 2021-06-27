# pacs
[![R build status](https://github.com/polkas/pacs/workflows/R-CMD-check/badge.svg)](https://github.com/polkas/pacs/actions)
[![CRAN](http://www.r-pkg.org/badges/version/pacs)](https://cran.r-project.org/package=pacs)
[![codecov](https://codecov.io/gh/Polkas/pacs/branch/master/graph/badge.svg)](https://codecov.io/gh/Polkas/pacs)

R packages utils

Hint: `Version` variable is mostly a minimal required i.e. max(version1, version2 , ...)

- Checking packages dependencies with their versions. 
- Validating the library for possible wrong packages versions (what we have vs what we should have). 
- Exploring complexity of packages.

## True R package size

This function might be crucial before we push our package to R CRAN.
We could control the weight of our project.
Take into account that the size are appropriate for you system `Sys.info()`.
Installation with `install.packages` and some `devtools` functions might result in different package sizes.

### Package

Size of a package:

```r
cat(pacs::pac_size("stats")/10**6, "Mb", "\n")
```

True size of a package as taking into account its dependencies:

```r
cat(pacs::pac_true_size("stats")/10**6, "Mb", "\n")
```

Might be useful to check the number of dependencies too:

```r
pacs::pac_deps("stats")$Package
```

### Remote version/dependecies

The installation process is not always a smooth one.
Thus I am recommending such manual usage:

```r
 package <- "shiny"
 version <- "1.5.0"
 withr::with_temp_libpaths({
    devtools::install_version(
      package,
      version,
      force = TRUE,
      dependencies = FALSE,
      quiet = TRUE,
      upgrade = "always",
      repos = "http://cran.rstudio.com/"
    )
    pac_deps(package)
    # cat(pacs::pac_true_size(package)/10**6, "Mb", "\n")
  })
```

## Package dependencies and diffeneces between versions

```r
install.packages("shiny")
```

```r
# Providing more than tools::package_dependencies and packrat:::recursivePackageVersion
# It is closer to packrat:::recursivePackageVersion
# pacs::package_deps is providing the min required version for each package
# Use it to answer what we should have
res <- pacs::pac_deps("shiny")
res
attributes(res)
```

Sth new on the R market.
comparing dependencies per package versions.

```r
# It was used withr::with_temp_libpaths and devtools::install_version for this task

pacs::pac_compare_versions("shiny", "1.4.0-2", "1.5.0")

pacs::pac_compare_versions("shiny", "1.4.0-2", "1.6.0")
```

## Packages dependencies

```r
all_deps <- pacs_deps()
```

```r
pacs_deps(c("stats", "shiny"))
```

## Globally - What we have vs What we should have 

```r
# Test with adding older cachem than extected
withr::with_temp_libpaths({
devtools::install_version("cachem", "1.0.0")
pacs::validate_lib()
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
