# 0.2.11

* New CRAN release.

# 0.2.10

*  Using `https://cran.r-project.org/web/checks/check_summary_by_package.html` to efficiently check the CRAN check pages globally. As a result `lib_validate` is hugely more efficient when `checkred` argument is triggered.
* Remove `mclapply` `README` examples, add notes that parallel computations might be unstable.
* Remove parallel computation from `pacs::lib_validate` as might be unstable and is already optimized.
* Added `checked_packages()` to retrieve the html table from `https://cran.r-project.org/web/checks/check_summary_by_package.html`.
* Removed `pac_comapre_exports` and replace with more general `pac_comapre_namespace`.

# 0.2.9

* Fixed `pac_lifeduration`, not work for old packages which do not have UTC in published Date.
* Added optional `FAIL` status when checking `CRAN` check pages.
* `checkred` argument from `lib_validate` will expecting any values from `c("ERROR", "FAIL", "WARN", "NOTE")` vector.
* Default `scope` for `pac_checkred` will be `c("ERROR", "FAIL")`.
* Improve `pac_compare_versions`, e.g. Default old version is the local one and the new one is the last release.
* Added `pac_comapre_exports` and `pac_namespace`.

# 0.2.8

* Added `pac_last` to check the most recent package version.
* Added additional usage example `pacs::pac_compare_versions` in `README` file. 
* CRAN version 0.2.8 .

# 0.2.7

* Removed all `pacs` functions, to give somebody the freedom of using different loop functions.
* Added a hint to use `mclapply` for non Windows users.
* Use `vapply` over the `parallel::mclapply` to be sure about the result length.
* Added additional `as.Date` so on older R versions binding is correct.

# 0.2.6

* Replace `gregexec` with a `stringi` function, as not supported on older R versions.
* Improved performance of `lib_validate` function, under default arguments and whole R CRAN library will consume 2 seconds.
* Update `roxygen2` descriptions.
* Added `lib.loc` and `repos` arguments to more functions.
* `mclapply` under many functions.

# 0.2.5

* Cache results only for 1 hour, could be important when run on servers.
* Add notice about caching results for 1 hour across all connected functions.
* Add additional description for validation function, result structure.
* Change the order in `README` file.
* Optional `lifeduration` and `checkred` for all validation functions.

# 0.2.4

* Polish descriptions.
* Deployment to R CRAN.
* Update `NEWS` file.

# 0.2.3

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

# 0.2.2

* Fixed `pac_deps` when `description_v = TRUE`, minimal required versions were taken from all local DESCRIPTION files. This will fix `pac_validate`/`pacs_validate` too, which were to optimistic.

# 0.2.1

* Added `https` for all URL.

# 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Added useful connected packages to Suggests.
* Added `roxygen2` to all exported functions.
* Achieved 80% of coverage.
* Written a clear `README` file.
* Removed all Imports from the `DESCRIPTION` file.
* Added `memoise` dependency to reduce the number of web calls.
* Created a decent basket of `pac`/`pacs` prefix functions.
* Removed all `\dontrun` calls.

