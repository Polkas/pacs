# Compare NAMESPACE exports between specific CRAN packages versions

using the remote github CRAN mirror to compare NAMESPACE exports between
specific packages versions.

## Usage

``` r
pac_compare_namespace(
  pac,
  old = NULL,
  new = NULL,
  lib.loc = .libPaths(),
  repos = "https://cran.rstudio.com/"
)
```

## Arguments

- pac:

  `character` a package name.

- old:

  `character` an old version of package, default local version. Default:
  `NULL`

- new:

  `character` a new version of package, default newest version. Default:
  `NULL`

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- repos:

  `character` vector repositories URLs to use. Used only for the
  validation. Default `https://cran.rstudio.com/`

## Value

`list` with
`c("imports", "exports", "exportPatterns", "importClasses", "importMethods", "exportClasses", "exportMethods", "exportClassPatterns", "dynlibs", "S3methods")`
slots, and added and removed ones for each of them.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_compare_namespace("shiny", "1.0.0", "1.6.0")
pacs::pac_compare_namespace("shiny", "1.0.0", "1.6.0")$exports
# local version to newest one
pacs::pac_compare_namespace("shiny")
} # }
```
