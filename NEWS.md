# pacs 0.4.3

* added an additional source for getting all releases of packages, the `metacran` db (new default).
* added a new validation function, `lock_validate`. It helps to validate a `renv` lock file.
* more efficient check of the Internet connection with the `curl` package.
* more elegant separation of the code.
* more testthat tests, `mockery`.

# pacs 0.4.2

* fix one of the validation of the `lib_validate` function.
* cache results with `memoise` for 30 minutes, not 1 hour.
* small README update.

# pacs 0.4.1

* `pacs::biocran_repos` not includes the `BioCbooks` field for `Bioc` versions smaller than "3.12".
* improve `pac_deps`, NA versions for not installed packages.
* updated `fields` argument of the`pac_deps` function, now has the same shape as `which` argument in the `tools::package_dependencies` function.
* more tests, 90 percent coverage.
* More clear distinction when working without Internet.

# pacs 0.4.0

* `pac_deps` fully depends on `tools::package_dependencies`.
* fix `pac_deps` problems when Suggests field is used, showed only installed packages.
* expose the `dir_size` function.
* add new functions `app_size` and `app_deps`.
* "getting started" vignette instead of the long README.
* get back functions reference table in the README.
* new Imports dependency, `renv`.

# pacs 0.3.8

* fix testthat tests.
* skip an unstable test on CRAN, a new package could be installed on the server during the check is running.
* accepting `"Enhances"` field in all related functions.
* improve the code for reading html tables.
* updated README file.

# pacs 0.3.7

* fixed a bug for `lib_validate` when a custom library path is used.
* updated the default value of the `lib.loc` argument from `NULL` to `.libPaths()`, as it will be more suited when using `withr` package. 
* updated README file.
* accepting `"Enhances"` field when checking dependencies.

# pacs 0.3.6 

* fix testthat tests to be more stable when dependency packages are updated.

# pacs 0.3.5

* Fixed `pac_timemachine` when from and to arguments are used and to is bigger than last Archived Date.
* `pac_timemachine` has a well ordered `rownames` now.
* Fixed `pac_compare_namespace`, how S3methods are handled.
* Fixed `pac_health` and related when the at argument is used and a package is installed locally.

# pacs 0.3.4

* `pac_deps` might show duplicated values if package was installed under a few different `libPaths`, now it is fixed.
* `lib_validate` will show packages which are required by DESCRIPTION file and not exist in `installed.packages`. This might occur when e.g. `install.packages` is corrupted. `pac_validate` already has such functionality.
* Update `roxygen2` of `pac_comapre_namespace` as the note field wrongly suggested that it works only for exports.
* remove "packages versions" section from README file.
* `pac_validate` will return `data.frame` with additional column "direct", which will point the direct dependency from DESCRIPTION file.
* Improve testthat tests for `pac_validate`.
* Improve README file for `lib_validate`.

# pacs 0.3.3

* Partly support BioConductor.
* Default repositories mainly now consists of not only CRAN as BioConductor is considered too.
* Fix `lib_validate` as it not take into account statuses for not fully performed tests, it was case for less than 1% of all packages.
* Add note that `checked_packages` could return duplicated observations which is expected.
* Improve merge in `lib_validate` to show non-CRAN packages too.
* Rewrite some tests as some R CRAN servers might shut down.
* Polishing code of `pac_lifeduration`.

# pacs 0.3.2

* New CRAN release.
* Added `\dontrun` to most of examples as `utils::installed.packages` consumes 30 seconds on win-builder.

# pacs 0.3.1

* Small docs improvements.
* Additional tests, maintain coverage over 80%.
* Additional input validation.
* Added `xml2` to imports.
* Update `README` file.

# pacs 0.3.0

* Added `pac_checkpage` and `cran_flavors`.
* Added `flavors` argument to functions checking packages statuses so only specific server might be considered.
* New `checkred` argument definition for `validate` family functions.
* Remove `dontrun` from `lib_validate`.
* Improving docs and descriptions.

# pacs 0.2.10

*  Using `https://cran.r-project.org/web/checks/check_summary_by_package.html` to efficiently check the CRAN check pages globally. As a result `lib_validate` is hugely more efficient when `checkred` argument is triggered.
* Remove `mclapply` `README` examples, add notes that parallel computations might be unstable.
* Remove parallel computation from `pacs::lib_validate` as might be unstable and is already optimized.
* Added `checked_packages()` to retrieve the html table from `https://cran.r-project.org/web/checks/check_summary_by_package.html`, all CRAN checks.
* Removed `pac_comapre_exports` and replace with more general `pac_comapre_namespace`.

# pacs 0.2.9

* Fixed `pac_lifeduration`, not work for old packages which do not have UTC in published Date.
* Added optional `FAIL` status when checking `CRAN` check pages.
* `checkred` argument from `lib_validate` will expecting any values from `c("ERROR", "FAIL", "WARN", "NOTE")` vector.
* Default `scope` for `pac_checkred` will be `c("ERROR", "FAIL")`.
* Improve `pac_compare_versions`, e.g. Default old version is the local one and the new one is the last release.
* Added `pac_comapre_exports` and `pac_namespace`.

# pacs 0.2.8

* Added `pac_last` to check the most recent package version.
* Added additional usage example `pacs::pac_compare_versions` in `README` file. 
* CRAN version 0.2.8 .

# pacs 0.2.7

* Removed all `pacs` functions, to give somebody the freedom of using different loop functions.
* Added a hint to use `mclapply` for non Windows users.
* Use `vapply` over the `parallel::mclapply` to be sure about the result length.
* Added additional `as.Date` so on older R versions binding is correct.

# pacs 0.2.6

* Replace `gregexec` with a `stringi` function, as not supported on older R versions.
* Improved performance of `lib_validate` function, under default arguments and whole R CRAN library will consume 2 seconds.
* Update `roxygen2` descriptions.
* Added `lib.loc` and `repos` arguments to more functions.
* `mclapply` under many functions.

# pacs 0.2.5

* Cache results only for 1 hour, could be important when run on servers.
* Add notice about caching results for 1 hour across all connected functions.
* Add additional description for validation function, result structure.
* Change the order in `README` file.
* Optional `lifeduration` and `checkred` for all validation functions.

# pacs 0.2.4

* Polish descriptions.
* Deployment to R CRAN.
* Update `NEWS` file.

# pacs 0.2.3

* Updated DESCRIPTION file description.
* Updated `README` file.
* Fixed `pac_true_size` with used `exclude_joint` argument, should not count checked package dependencies.
* Secured against duplicates in `pac_compare_versions`.
* Removed the "Description" column from a `pac_timemachine` result.
* Added the `reverse` argument for `pac_deps` which working for description versions too.
* Added the `repos` argument for `pac_deps`.
* Remove `base` argument in `pac_true_size`, as not see any value added.
* Add `checkred` variable for validation functions.
* `pac_health` for newest release younger than x days, checking if package is red labeled on CRAN checks pages.
* Default 14 days as limit for valid version, and non red check for the newest version.
* Added new functions `pac_checkred`/`pacs_checkred`.

# pacs 0.2.2

* Fixed `pac_deps` when `description_v = TRUE`, minimal required versions were taken from all local DESCRIPTION files. This will fix `pac_validate`/`pacs_validate` too, which were to optimistic.

# pacs 0.2.1

* Added `https` for all URL.

# pacs 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Added useful connected packages to Suggests.
* Added `roxygen2` to all exported functions.
* Achieved 80% of coverage.
* Written a clear `README` file.
* Removed all Imports from the `DESCRIPTION` file.
* Added `memoise` dependency to reduce the number of web calls.
* Created a decent basket of `pac`/`pacs` prefix functions.
* Removed all `\dontrun` calls.

