# Get base R packages

get base packages, all or only `startup`.

## Usage

``` r
pacs_base(startup = FALSE)
```

## Arguments

- startup:

  `logical` include only `startup` packages. Default: `FALSE`

## Value

`character` vector

## Examples

``` r
if (FALSE) { # \dontrun{
pacs_base()
pacs_base(startup = TRUE)
} # }
```
