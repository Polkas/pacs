# Size of the package

size of package.

## Usage

``` r
dir_size(path = ".", recursive = TRUE)
```

## Arguments

- path:

  `character` path to the shiny app. Default: `"."`

- recursive:

  `logical` if to assess the dependencies recursively. Default: TRUE

## Value

`numeric` size in bytes, to get MB ten divide by `10**6`.

## Examples

``` r
if (FALSE) { # \dontrun{
cat(pacs::dir_size(system.file(package = "stats")) / 10**6, "MB")
} # }
```
