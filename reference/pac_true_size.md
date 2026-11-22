# True size of the package

True size of the package as it takes into account its all dependencies,
recursively.

## Usage

``` r
pac_true_size(
  pac,
  fields = c("Depends", "Imports", "LinkingTo"),
  lib.loc = .libPaths(),
  exclude_joint = 0L
)
```

## Arguments

- pac:

  `character` a package name.

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

- exclude_joint:

  `integer` exclude packages which are dependencies of at least N other
  packages, not count main package dependencies. Default: `0`

## Value

`numeric` size in bytes, to get MB then divide by `10**6`.

## Note

R base packages are not counted. The default value of `fields` should be
suited for almost all scenarios.

## Examples

``` r
if (FALSE) { # \dontrun{
# size in MB, with all its dependencies
pacs::pac_true_size("memoise") / 10**6
} # }
```
