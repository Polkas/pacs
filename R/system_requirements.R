#' Package system requirements
#'
#' Report the `SystemRequirements` fields declared by a package and its
#' dependencies. These fields are free text and can describe optional or
#' platform-specific requirements; this function does not check whether they
#' are installed or resolve dependency version constraints. It reports the
#' installed versions or the versions selected from the repository indexes.
#'
#' With `recursive = FALSE`, only the package and its direct dependencies are
#' considered. With `recursive = TRUE`, dependencies are followed recursively.
#' By default, `Suggests` and `Enhances` are only followed for the requested
#' package when included in `fields`; indirect dependencies use
#' `recursive_fields`.
#' R and its base packages are excluded from the dependencies, but the
#' requested package is always included.
#'
#' @inheritParams standard_args
#' @param fields Dependency fields to follow for the requested package. In
#'   addition to the values
#'   described in [pac_deps()], `character(0)` considers only the requested
#'   package.
#' @param local Use installed packages when `TRUE` (the default), or current
#'   source packages in `repos` when `FALSE`.
#' @param recursive Follow dependencies recursively. Default: `TRUE`.
#' @param recursive_fields Dependency fields to follow for indirect
#'   dependencies. Defaults to `c("Depends", "Imports", "LinkingTo")`.
#'   Accepts the same values as `fields`, including `character(0)`.
#' @param repos Repositories to use for source packages when `local = FALSE`.
#'   Default: [biocran_repos()]. Bulk repository metadata is used when
#'   available; otherwise source archives are downloaded to read their
#'   DESCRIPTION files.
#' @return A named `list`, similar to [tools::package_dependencies()]. Names
#'   are package names, starting with `pac`, and each element is a `character`
#'   vector of declared system requirements. The `SystemRequirements` field is
#'   split on top-level commas and semicolons (not inside parentheses), with
#'   whitespace normalized; fields written as sentences are kept whole.
#'   Splitting is heuristic because the field is free text. Packages without
#'   a declared field have
#'   `character(0)`. Dependencies missing from the library or repositories
#'   have `NA_character_` and produce a warning, because their own
#'   dependencies cannot be assessed. The `"versions"` attribute is a named
#'   `character` vector with the installed or repository versions.
#'   Use `unique(unlist(result))` to get all requirements.
#' @note By default, source repository indexes are filtered for the current
#'   R version and platform. Bulk metadata and downloaded DESCRIPTION fields
#'   are cached for 30 minutes. A missing `SystemRequirements` field does not
#'   guarantee that a package has no system requirements. Bioconductor bulk
#'   metadata can normalize whitespace in the declared field.
#' @export
#' @examples
#' pac_system_requirements("stats", fields = character())
#' \dontrun{
#' pac_system_requirements("curl", recursive = FALSE)
#' pac_system_requirements("sf", local = FALSE, repos = "https://cran.r-project.org")
#' sf <- pac_system_requirements("sf", fields = "most", local = FALSE)
#' sf[["sf"]]
#' # all requirements of sf and its dependencies
#' unique(unlist(sf, use.names = FALSE))
#' }
pac_system_requirements <- function(
  pac,
  fields = c("Depends", "Imports", "LinkingTo"),
  lib.loc = .libPaths(),
  local = TRUE,
  recursive = TRUE,
  recursive_fields = c("Depends", "Imports", "LinkingTo"),
  repos = biocran_repos()
) {
  if (!is.character(pac) || length(pac) != 1L || is.na(pac) ||
      !grepl("^[A-Za-z][A-Za-z0-9.]*$", pac)) {
    stop("`pac` must be a single valid package name.", call. = FALSE)
  }
  stopifnot(is.character(fields), !anyNA(fields))
  fields <- expand_dependency(fields)
  stopifnot(is.character(recursive_fields), !anyNA(recursive_fields))
  recursive_fields <- expand_dependency(recursive_fields)
  stopifnot(is.logical(local), length(local) == 1L, !is.na(local))
  stopifnot(is.logical(recursive), length(recursive) == 1L, !is.na(recursive))

  if (local) {
    stopifnot(is.null(lib.loc) ||
      (is.character(lib.loc) && !anyNA(lib.loc) && all(dir.exists(lib.loc))))
    packages <- utils::installed.packages(
      lib.loc = lib.loc,
      fields = "SystemRequirements"
    )
    source <- "the requested libraries"
  } else {
    stopifnot(is.character(repos), length(repos) > 0L, !anyNA(repos), all(nzchar(repos)))
    packages <- available_packages_raw(repos = repos, type = "source")
    source <- "the provided source repositories"
  }
  packages <- packages[!duplicated(packages[, "Package"]), , drop = FALSE]

  if (!pac %in% packages[, "Package"]) {
    stop(sprintf("Package '%s' was not found in %s.", pac, source), call. = FALSE)
  }

  dependencies <- if (length(fields) == 0L) {
    character()
  } else {
    tools::package_dependencies(
      pac, db = packages, which = fields, recursive = FALSE
    )[[1L]]
  }
  if (recursive && length(recursive_fields) > 0L) {
    seen <- unique(c(pac, dependencies))
    pending <- setdiff(dependencies, pac)
    while (length(pending) > 0L) {
      next_level <- tools::package_dependencies(
        pending, db = packages, which = recursive_fields, recursive = FALSE
      )
      pending <- setdiff(unlist(next_level, use.names = FALSE), seen)
      seen <- c(seen, pending)
    }
    dependencies <- setdiff(seen, pac)
  }
  dependencies <- setdiff(dependencies, pacs_base())
  selected <- unique(c(pac, sort(dependencies)))
  positions <- match(selected, packages[, "Package"])
  missing <- selected[is.na(positions)]
  if (length(missing) > 0L) {
    warning(
      sprintf(
        "Dependencies not found in %s: %s. Their own dependencies cannot be assessed.",
        source, paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  versions <- unname(packages[positions, "Version"])
  requirements <- if (local) {
    unname(packages[positions, "SystemRequirements"])
  } else {
    available <- which(!is.na(positions))
    repositories <- unname(packages[positions[available], "Repository"])
    if (anyNA(repositories) || any(!nzchar(repositories))) {
      stop("The source repository index contains a missing repository.", call. = FALSE)
    }
    if (anyNA(versions[available]) ||
        any(!grepl("^[0-9][0-9.-]*$", versions[available]))) {
      stop("The source repository index contains an invalid package version.", call. = FALSE)
    }

    requirements <- rep(NA_character_, length(selected))
    for (repository in unique(repositories)) {
      rows <- available[repositories == repository]
      metadata <- repository_bulk_system_requirements(repository)
      matches <- if (is.null(metadata)) {
        rep(NA_integer_, length(rows))
      } else {
        match(
          paste(selected[rows], versions[rows], sep = "\n"),
          paste(metadata$Package, metadata$Version, sep = "\n")
        )
      }
      found <- !is.na(matches)
      if (any(found)) {
        requirements[rows[found]] <- metadata$SystemRequirements[matches[found]]
      }
      fallback <- rows[!found]
      if (length(fallback) >= 50L) {
        message(sprintf(
          "Bulk metadata is unavailable for %s packages from %s; downloading their source archives.",
          length(fallback), repository
        ))
      }
      for (i in fallback) {
        requirements[i] <- repository_system_requirements(selected[i], versions[i], repository)
      }
    }
    requirements
  }

  result <- lapply(requirements, split_system_requirements)
  result[is.na(positions)] <- list(NA_character_)
  names(result) <- selected
  attr(result, "versions") <- stats::setNames(versions, selected)
  result
}

split_system_requirements <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(character())
  }
  x <- trimws(gsub("[[:space:]]+", " ", x))
  # Prose descriptions are kept whole; only list-like fields are split.
  if (grepl("\\.( |$)", gsub("\\([^()]*\\)", "", x))) {
    return(if (nzchar(x)) x else character())
  }
  parts <- trimws(strsplit(x, "[,;](?![^()]*\\))", perl = TRUE)[[1L]])
  parts[nzchar(parts)]
}

repository_metadata_content <- function(url) {
  if (startsWith(url, "file://")) {
    path <- substring(url, 8L)
    if (.Platform$OS.type == "windows" && grepl("^/[A-Za-z]:/", path)) {
      path <- substring(path, 2L)
    }
    if (!file.exists(path)) {
      return(NULL)
    }
    return(readBin(path, what = "raw", n = file.info(path)$size))
  }
  if (!grepl("^https?://", url)) {
    return(NULL)
  }

  response <- tryCatch(
    curl::curl_fetch_memory(
      url, handle = curl::new_handle(timeout = getOption("timeout", 60))
    ),
    error = function(e) {
      stop(sprintf("Could not retrieve bulk package metadata from %s: %s",
        url, conditionMessage(e)
      ), call. = FALSE)
    }
  )
  if (response$status_code %in% c(404L, 410L)) {
    return(NULL)
  }
  if (response$status_code %in% c(401L, 403L)) {
    warning(sprintf(
      "Bulk package metadata at %s returned HTTP %s; using source archives instead.",
      url, response$status_code
    ), call. = FALSE)
    return(NULL)
  }
  if (response$status_code != 200L) {
    stop(sprintf("Could not retrieve bulk package metadata from %s: HTTP %s.",
      url, response$status_code
    ), call. = FALSE)
  }
  response$content
}

repository_bulk_system_requirements_raw <- function(repository) {
  bioc <- regmatches(
    repository,
    regexec(
      "^(.*)/packages/([0-9]+[.][0-9]+)/(bioc|data/annotation|data/experiment|workflows|books)/src/contrib/?$",
      repository
    )
  )[[1L]]
  if (length(bioc) > 0L) {
    url <- sprintf("%s/packages/json/%s/%s/packages.json", bioc[2L], bioc[3L], bioc[4L])
  } else {
    root <- sub("/src/contrib/?$", "", repository)
    if (identical(root, repository)) {
      return(NULL)
    }
    url <- paste0(root, "/web/packages/packages.rds")
  }
  content <- repository_metadata_content(url)
  if (is.null(content)) {
    return(NULL)
  }

  tryCatch(
    {
      if (length(bioc) > 0L) {
        records <- jsonlite::fromJSON(rawToChar(content), simplifyVector = FALSE)
        if (!is.list(records) || (length(records) > 0L && is.null(names(records))) ||
            anyDuplicated(names(records))) {
          stop("Expected a named list of Bioconductor package records.")
        }
        field <- function(name, required = TRUE) {
          vapply(records, function(record) {
            if (!is.list(record)) {
              stop("A Bioconductor package record is not an object.")
            }
            value <- record[[name]]
            if (is.null(value) && !required) {
              return(NA_character_)
            }
            if (!is.character(value) || length(value) != 1L || is.na(value) ||
                (required && !nzchar(value))) {
              stop(sprintf("Invalid '%s' in a Bioconductor package record.", name))
            }
            value
          }, character(1), USE.NAMES = FALSE)
        }
        metadata <- data.frame(
          Package = field("Package"),
          Version = field("Version"),
          SystemRequirements = field("SystemRequirements", required = FALSE),
          stringsAsFactors = FALSE
        )
        if (length(records) > 0L && !identical(names(records), metadata$Package)) {
          stop("Bioconductor package names do not match their metadata records.")
        }
      } else {
        connection <- gzcon(rawConnection(content, open = "rb"))
        on.exit(close(connection), add = TRUE)
        records <- readRDS(connection)
        fields <- c("Package", "Version", "SystemRequirements")
        if (!(is.matrix(records) || is.data.frame(records)) ||
            !all(fields %in% colnames(records))) {
          stop("Expected package, version, and system requirements columns.")
        }
        metadata <- data.frame(
          Package = as.character(records[, "Package"]),
          Version = as.character(records[, "Version"]),
          SystemRequirements = as.character(records[, "SystemRequirements"]),
          stringsAsFactors = FALSE
        )
      }
      if (anyNA(metadata$Package) || anyNA(metadata$Version) ||
          any(!nzchar(metadata$Package)) || any(!nzchar(metadata$Version))) {
        stop("Bulk metadata contains missing package names or versions.")
      }
      keys <- paste(metadata$Package, metadata$Version, sep = "\n")
      duplicated_keys <- unique(keys[duplicated(keys)])
      if (length(duplicated_keys) > 0L) {
        conflicting <- duplicated_keys[vapply(duplicated_keys, function(key) {
          length(unique(metadata$SystemRequirements[keys == key])) > 1L
        }, logical(1))]
        if (length(conflicting) > 0L) {
          warning(sprintf(
            "Bulk metadata from %s has conflicting requirements for %s package versions; their source archives will be used.",
            url, length(conflicting)
          ), call. = FALSE)
        }
        metadata <- metadata[!duplicated(keys) & !keys %in% conflicting, , drop = FALSE]
        rownames(metadata) <- NULL
      }
      metadata
    },
    error = function(e) {
      stop(sprintf("Invalid bulk package metadata from %s: %s",
        url, conditionMessage(e)
      ), call. = FALSE)
    }
  )
}

repository_bulk_system_requirements <- memoise::memoise(
  repository_bulk_system_requirements_raw,
  cache = cachem::cache_mem(max_age = 30 * 60)
)

repository_system_requirements_raw <- function(pac, version, repository) {
  if (!is.character(pac) || length(pac) != 1L || is.na(pac) ||
      !grepl("^[A-Za-z][A-Za-z0-9.]*$", pac)) {
    stop("Invalid package name in the repository index.", call. = FALSE)
  }
  if (!is.character(version) || length(version) != 1L || is.na(version) ||
      !grepl("^[0-9][0-9.-]*$", version)) {
    stop(sprintf("Invalid repository version for package '%s'.", pac), call. = FALSE)
  }
  if (!is.character(repository) || length(repository) != 1L ||
      is.na(repository) || !nzchar(repository)) {
    stop(sprintf("Missing repository for package '%s'.", pac), call. = FALSE)
  }
  archive <- tempfile(fileext = ".tar.gz")
  directory <- tempfile()
  if (!dir.create(directory)) {
    stop("Could not create a temporary directory for package metadata.", call. = FALSE)
  }
  on.exit(unlink(c(archive, directory), recursive = TRUE), add = TRUE)

  url <- sprintf(
    "%s/%s_%s.tar.gz", sub("/+$", "", repository), pac, version
  )
  tryCatch(
    {
      status <- utils::download.file(url, archive, mode = "wb", quiet = TRUE)
      if (status != 0L) {
        stop(sprintf("Download returned status %s.", status))
      }
      utils::untar(
        archive, files = paste0(pac, "/DESCRIPTION"),
        exdir = directory, tar = "internal"
      )
      description <- file.path(directory, pac, "DESCRIPTION")
      if (!file.exists(description)) {
        stop("The source archive does not contain its DESCRIPTION file.")
      }
      metadata <- read.dcf(
        description, fields = c("Package", "Version", "SystemRequirements")
      )
      if (nrow(metadata) != 1L || is.na(metadata[1L, "Package"]) ||
          is.na(metadata[1L, "Version"]) || metadata[1L, "Package"] != pac ||
          metadata[1L, "Version"] != version) {
        stop("The DESCRIPTION package or version does not match the repository index.")
      }
      unname(metadata[1L, "SystemRequirements"])
    },
    error = function(e) {
      stop(
        sprintf("Could not read DESCRIPTION for %s %s from %s: %s",
          pac, version, repository, conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

repository_system_requirements <- memoise::memoise(
  repository_system_requirements_raw,
  cache = cachem::cache_mem(max_age = 30 * 60)
)
