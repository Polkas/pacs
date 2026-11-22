# Minimum version across the vector

Reduce function over the
[`utils::compareVersion`](https://rdrr.io/r/utils/compareVersion.html)

## Usage

``` r
compareVersionsMin(vec, na.rm = TRUE)
```

## Arguments

- vec:

  `character` vector.

- na.rm:

  `logical` if to remove `NA` values.

## Value

`character` minimal version

## Examples

``` r
compareVersionsMin(c("1.1.1", "0.2.0"))
#> [1] "0.2.0"
```
