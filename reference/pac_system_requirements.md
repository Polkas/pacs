# Package system requirements

Report the `SystemRequirements` fields declared by a package and its
dependencies. These fields are free text and can describe optional or
platform-specific requirements; this function does not check whether
they are installed or resolve dependency version constraints. It reports
the installed versions or the versions selected from the repository
indexes.

## Usage

``` r
pac_system_requirements(
  pac,
  fields = c("Depends", "Imports", "LinkingTo"),
  lib.loc = .libPaths(),
  local = TRUE,
  recursive = TRUE,
  recursive_fields = c("Depends", "Imports", "LinkingTo"),
  repos = biocran_repos()
)
```

## Arguments

- pac:

  `character` a package name.

- fields:

  Dependency fields to follow for the requested package. In addition to
  the values described in
  [`pac_deps()`](https://polkas.github.io/pacs/reference/pac_deps.md),
  `character(0)` considers only the requested package.

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- local:

  Use installed packages when `TRUE` (the default), or current source
  packages in `repos` when `FALSE`.

- recursive:

  Follow dependencies recursively. Default: `TRUE`.

- recursive_fields:

  Dependency fields to follow for indirect dependencies. Defaults to
  `c("Depends", "Imports", "LinkingTo")`. Accepts the same values as
  `fields`, including `character(0)`.

- repos:

  Repositories to use for source packages when `local = FALSE`. Default:
  [`biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md).
  Bulk repository metadata is used when available; otherwise source
  archives are downloaded to read their DESCRIPTION files.

## Value

A named `list`, similar to
[`tools::package_dependencies()`](https://rdrr.io/r/tools/package_dependencies.html).
Names are package names, starting with `pac`, and each element is a
`character` vector of declared system requirements. The
`SystemRequirements` field is split on top-level commas and semicolons
(not inside parentheses), with whitespace normalized; fields written as
sentences are kept whole. Splitting is heuristic because the field is
free text. Packages without a declared field have `character(0)`.
Dependencies missing from the library or repositories have
`NA_character_` and produce a warning, because their own dependencies
cannot be assessed. The `"versions"` attribute is a named `character`
vector with the installed or repository versions. Use
`unique(unlist(result))` to get all requirements.

## Details

With `recursive = FALSE`, only the package and its direct dependencies
are considered. With `recursive = TRUE`, dependencies are followed
recursively. By default, `Suggests` and `Enhances` are only followed for
the requested package when included in `fields`; indirect dependencies
use `recursive_fields`. R and its base packages are excluded from the
dependencies, but the requested package is always included.

## Note

By default, source repository indexes are filtered for the current R
version and platform. Bulk metadata and downloaded DESCRIPTION fields
are cached for 30 minutes. A missing `SystemRequirements` field does not
guarantee that a package has no system requirements. Bioconductor bulk
metadata can normalize whitespace in the declared field.

## Examples

``` r
pac_system_requirements("stats", fields = character())
#> $stats
#> character(0)
#> 
#> attr(,"versions")
#>   stats 
#> "4.6.2" 
if (FALSE) { # \dontrun{
pac_system_requirements("curl", recursive = FALSE)
pac_system_requirements("sf", local = FALSE, repos = "https://cran.r-project.org")
sf <- pac_system_requirements("sf", fields = "most", local = FALSE)
sf[["sf"]]
# all requirements of sf and its dependencies
unique(unlist(sf, use.names = FALSE))
} # }
```
