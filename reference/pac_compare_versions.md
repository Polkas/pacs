# Compare DESCRIPTION files dependencies between specific CRAN packages versions

using the remote github CRAN mirror to compare DESCRIPTION files
dependencies between specific packages versions.

## Usage

``` r
pac_compare_versions(
  pac,
  old = NULL,
  new = NULL,
  fields = c("Imports", "Depends", "LinkingTo"),
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

- fields:

  `character` vector listing the types of dependencies, a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
  Character string "all" is shorthand for that vector, character string
  "most" for the same vector without "Enhances", character string
  "strong" (default) for the first three elements of that vector.
  Default: `c("Depends", "Imports", "LinkingTo")`

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- repos:

  `character` vector repositories URLs to use. Used only for the
  validation. Default `https://cran.rstudio.com/`

## Value

`data.frame` with 4 columns.

- Package:

  character package names.

- Version.OLD:

  character versions of dependencies required by an old package version.

- Version.NEW:

  character versions of dependencies required by a new package version.

- version_status:

  numeric -1/0/1 which comes from
  [`utils::compareVersion`](https://rdrr.io/r/utils/compareVersion.html)
  function. 0 means that both versions have the same requirement. -1
  means that the new version remove this requirement. 1 means that the
  new version added a new requirement.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_compare_versions("memoise", "0.2.1", "2.0.0")
pacs::pac_compare_versions("memoise", "0.2.1")
# local version to newest one
pacs::pac_compare_versions("memoise")
} # }
```
