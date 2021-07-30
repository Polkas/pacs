# 0.2.8

* Added `pac_last` to check the most recent package version.
* Added additional usage example `pacs::pac_compare_versions` in `README` file. 
* CRAN version 0.2.8 .

# 0.2.7

* Removed all `pacs` functions, to give somebody the freedom of using different loop functions.
* Added a hint to use `mclapply` for non Windows users.
* Use `vapply` over the `parallel::mclapply` to be sure about the result length.
* Added additional `ad.Date` so on older R versions binding is correct.

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

