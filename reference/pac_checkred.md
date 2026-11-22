# Checking the R CRAN package check page status

using package R CRAN check page to validate if there are ANY errors
and/or fails and/or warnings and/or notes.

## Usage

``` r
pac_checkred(pac, scope = c("ERROR", "FAIL"), flavors = NULL)
```

## Arguments

- pac:

  `character` a package name.

- scope:

  `character` vector scope of the check, accepted values c("ERROR",
  "FAIL", "WARN", "NOTE"). Default: `c("ERROR", "FAIL")`

- flavors:

  `character` vector of CRAN server names to consider, possible names
  could be get with `pacs::cran_flavors()$Flavor`. The
  [`pacs::match_flavors()`](https://polkas.github.io/pacs/reference/match_flavors.md)
  function could be used to get CRAN server names matched for your local
  `OS`. By default all CRAN machines are considered `NULL` value.
  Default: `NULL`

## Value

`logical` if the package fails under the specified criteria, or `NA` if
the required check statuses cannot be retrieved.

## Note

Results are cached for 30 minutes with `memoise` package. If you need to
check many packages at once then is recommended usage of
[`pacs::checked_packages`](https://polkas.github.io/pacs/reference/checked_packages.md).
The used repository `https://cran.rstudio.com/`. Please as a courtesy to
the R CRAN, don't overload their server by constantly using this
function.

## Examples

``` r
if (FALSE) { # \dontrun{
pacs::pac_checkred("dplyr")
pacs::pac_checkred("dplyr", scope = c("ERROR"))
pacs::pac_checkred("dplyr",
  scope = c("ERROR", "FAIL", "WARN"),
  flavors = pacs::match_flavors()
)
} # }
```
