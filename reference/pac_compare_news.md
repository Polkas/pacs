# Compare NEWS files between specific CRAN packages versions

using the remote github CRAN mirror to compare NEWS files between
specific packages versions.

## Usage

``` r
pac_compare_news(
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

`character` with NEWS content between specific versions, or `NA` if the
requested version headings cannot be found in release order.

## Note

Versions are matched against the first complete version number in each
Markdown heading.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_compare_news("shiny", "1.0.0", "1.6.0")
# local version to newest one
pacs::pac_compare_news("shiny")
} # }
```
