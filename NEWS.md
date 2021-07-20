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

