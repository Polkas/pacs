# Size of the package

Size of package.

## Usage

``` r
pac_size(pac, lib.loc = .libPaths())
```

## Arguments

- pac:

  `character` a package name.

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

## Value

`numeric` size in bytes, to get MB ten divide by `10**6`.

## Examples

``` r
if (FALSE) { # \dontrun{
cat(pacs::pac_size("stats") / 10**6, "MB")
} # }
```
