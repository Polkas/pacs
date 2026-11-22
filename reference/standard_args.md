# Standard Template Arguments

Standard Template Arguments

## Arguments

- at:

  `Date` from which to take the version. Default: `NULL`

- attr:

  `logical` if a package and its version should be added as an attribute
  of data.frame or for FALSE as an additional record. Default: TRUE

- base:

  `logical` if to add base packages too. If `TRUE` then
  [`pacs::pacs_base()`](https://polkas.github.io/pacs/reference/pacs_base.md)
  are taken into account. Default: `FALSE`

- built:

  `logical` if to add an R version under which each package was
  installed. Useful mainly for a local usage. Packages installed with a
  previous version of R could not work correctly with the new version
  of R. Default: `FALSE`

- checkred:

  `list` with two named fields, `scope` and `flavor`. `scope` of R CRAN
  check pages statuses to consider, any of
  `c("ERROR", "FAIL", "WARN", "NOTE")`. `flavor` is a vector of CRAN
  machines to consider, which might be retrieved with
  `pacs::cran_flavors()$Flavor`. By default an empty scope field
  deactivated assessment for `checkred` column, and NULL flavor will
  results in checking all machines. Default:
  `list(scope = character(0), flavor = NULL)`

- description_v:

  `logical` if the dependencies version should be taken from description
  files, minimal required. By default installed versions are taken.
  Default: `FALSE`

- exclude_joint:

  `integer` exclude packages which are dependencies of at least N other
  packages, not count main package dependencies. Default: `0`

- file:

  `character` file name to read. Possible values are `DESCRIPTION`,
  `NEWS` and `NAMESPACE`.

- fields:

  `character` vector listing the types of dependencies, a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
  Character string "all" is shorthand for that vector, character string
  "most" for the same vector without "Enhances", character string
  "strong" (default) for the first three elements of that vector.
  Default: `c("Depends", "Imports", "LinkingTo")`

- flavors:

  `character` vector of CRAN server names to consider, possible names
  could be get with `pacs::cran_flavors()$Flavor`. The
  [`pacs::match_flavors()`](https://polkas.github.io/pacs/reference/match_flavors.md)
  function could be used to get CRAN server names matched for your local
  `OS`. By default all CRAN machines are considered `NULL` value.
  Default: `NULL`

- from:

  `Date` the lower limit. Default: `NULL`

- lib.loc:

  `character` vector of search paths with local packages. Default:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- lifeduration:

  `logical` if to assess life duration for each package in the library.
  For installed newest releases of packages, a local evaluation is used.
  `MEATCRAN CRANDB` is used for libraries with less than 500 packages.
  Otherwise the direct web page download from CRAN is used. Default:
  \`FALSE“

- limit:

  `numeric` at least days to treat as healthy, "\>=limit". Default: 14

- local:

  `logical` if to use local repository (or newest remote packages).
  Default: `FALSE`

- na.rm:

  `logical` if to remove `NA` values.

- new:

  `character` a new version of package, default newest version. Default:
  `NULL`

- old:

  `character` an old version of package, default local version. Default:
  `NULL`

- pac:

  `character` a package name.

- pacs:

  `character` vector of packages names.

- path:

  `character` path to the shiny app. Default: `"."`

- repos:

  `character` vector of repositories URLs to use. By default checking
  CRAN and newest Bioconductor per R version. Default
  [`pacs::biocran_repos()`](https://polkas.github.io/pacs/reference/biocran_repos.md)

- recursive:

  `logical` if to assess the dependencies recursively. Default: TRUE

- scope:

  `character` vector scope of the check, accepted values c("ERROR",
  "FAIL", "WARN", "NOTE"). Default: `c("ERROR", "FAIL")`

- source:

  `character` one of `c("cran", "crandb")`. Using the `MEATCRAN CRANDB`
  or the direct web page download from CRAN. When `"crandb"` is set then
  `options(pacs.crandb_ntry = 3)` and
  `options(pacs.crandb_nsleep = 0.001)` can be used to control the
  fetch. `"pacs.crandb_ntry"` can be used to set the maximum number of
  try outs, by default 3. `"pacs.crandb_nsleep"` can be used to set the
  sleep duration between fetch try outs, by default 0.001. Default:
  `"cran"`

- startup:

  `logical` include only `startup` packages. Default: `FALSE`

- to:

  `Date` the upper limit. Default: `NULL`

- vec:

  `character` vector.

- version:

  `character` version of a package. Default: `NULL`

- versions:

  `character` vector of packages versions.
