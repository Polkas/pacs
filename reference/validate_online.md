# Append a data.frame with online attributes

internal function to append a data.frame with an online related sources.
The input data.frame has a specific structure, such contain
`c("Package", version_name_new)` columns.

## Usage

``` r
validate_online(
  result,
  version_name_new = "Version.expected",
  lifeduration,
  checkred,
  repos,
  lib.loc = .libPaths()
)
```
