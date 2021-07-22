# 0.2.3

* Updated DESCRIPTION file description.
* Updated `README` file.
* Fixed `pac_true_size` with any `exclude_joint` argument, should always count base packages.
* Secured against duplicates in `pac_compare_versions`.
* Removed the "Description" column from a `pac_timemachine` result.
* Added the `reverse` argument for `pac_deps` which working for description versions too.
* Added the `repos` argument for `pac_deps`.
* Remove `base` argument in `pac_true_size`, as not see any value added.
* Add `is_red` variable for validation functions.
* `pac_health` for newest release younger than x days, checking if package is red labeled on R CRAN checks.
* Default 14 days as limit for healthy version, and non red check for the newest version.
* Added new functions `pac_checkred`/`pacs_checkred`.

# 0.2.2

* Fixed `pac_deps` when `description_v = TRUE`, minimal required versions were taken from all local DESCRIPTION files. This will fix `pac_validate`/`pacs_validate` too, which were to optimistic.

# 0.2.1

* Added `https` for all URL.

# 0.2.0

* Deployment to R CRAN.
* Added a `NEWS.md` file to track changes to the package.
* Added useful connected packages to Suggests.
* Added `roxygen2` to all exported functions.
* Achieved 80% of coverage.
* Written a clear `README` file.
* Removed all Imports from the `DESCRIPTION` file.
* Added `memoise` dependency to reduce the number of web calls.
* Created a decent basket of `pac`/`pacs` prefix functions.
* Removed all `\dontrun` calls.

