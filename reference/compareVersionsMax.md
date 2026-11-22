# Maximum version across the vector

Reduce function over the
[`utils::compareVersion`](https://rdrr.io/r/utils/compareVersion.html)

## Usage

``` r
compareVersionsMax(vec, na.rm = TRUE)
```

## Arguments

- vec:

  `character` vector.

- na.rm:

  `logical` if to remove `NA` values.

## Value

`character` maximum version

## Examples

``` r
compareVersionsMax(c("1.1.1", "0.2.0"))
#> [1] "1.1.1"
```
