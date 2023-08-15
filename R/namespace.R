#' package NAMESPACE file
#' @description CRAN package NAMESPACE file taken locally or remotely from GITHUB CRAN mirror or CRAN website.
#' By default works for the newest package version.
#' @inheritParams standard_args
#' @param repos `character` vector repositories URLs to use. Used only for the validation. Default `https://cran.rstudio.com/`
#' @return `list` with names proper for NAMESPACE file, the same as format as returned by `base::parseNamespaceFile`.
#' @note Results are cached for 30 minutes with `memoise` package.
#' This function is mainly built under source code from `base::parseNamespaceFile`.
#' @export
#' @examples
#' \dontrun{
#' pacs::pac_namespace("dplyr", version = "0.8.0")
#' pacs::pac_namespace("dplyr", at = as.Date("2019-02-01"))
#' pacs::pac_namespace("memoise", local = TRUE)
#' }
pac_namespace <- function(pac, version = NULL, at = NULL, local = FALSE, lib.loc = .libPaths(), repos = "https://cran.rstudio.com/") {
  stopifnot((isFALSE(local)) ||
    (isTRUE(local) && (is.null(version) || isTRUE(utils::packageDescription(pac, lib.loc = lib.loc)$Version == version))))
  stopifnot(all(c(is.null(version), is.null(at))) || xor(!is.null(version), !is.null(at)))
  stopifnot(is.null(at) || inherits(at, "Date"))
  stopifnot(length(pac) == 1 && is.character(pac))
  stopifnot(is.null(lib.loc) || (all(lib.loc %in% .libPaths()) && (length(list.files(lib.loc)) > 0)))
  stopifnot(is.null(version) || (length(version) == 1 && is.character(version)))

  is_installed <- isTRUE(pac %in% rownames(installed_packages(lib.loc = lib.loc)))
  if ((!is_installed && local) || (!local && !is_online())) {
    return(NA)
  }
  version_installed <- if (is_installed) {
    pac_description(pac, local = TRUE)$Version
  } else {
    NA
  }

  version_null <- is.null(version)

  if (local && is_installed && is.null(at) && (version_null || isTRUE(utils::compareVersion(version, version_installed) == 0))) {
    namespace_lines <- readLines(system.file(package = pac, "NAMESPACE"), warn = FALSE)
    desc <- pac_description(pac, local = TRUE)
  } else if (isTRUE(is_isin(pac, "https://cran.rstudio.com/"))) {
    version <- if (!version_null) {
      version
    } else if (!is.null(at)) {
      res <- pac_timemachine(pac, at = at)
      if (isNA(res) || is.null(res)) {
        return(NA)
      } else {
        utils::tail(res$Version, 1)
      }
    } else {
      pac_last(pac, repos = repos)
    }
    namespace_lines <- pac_readnamespace(pac, version, repos)
    desc <- pac_description(pac, version = version)
    if (isTRUE(is.na(namespace_lines))) {
      return(NA)
    }
    if (length(namespace_lines) == 0) {
      return(NA)
    }
  } else {
    return(NA)
  }
  enc <- desc$Encoding
  if (is.null(enc) || isFALSE(enc %in% c("UTF-8", "latin1", "latin2"))) enc <- "UTF-8"
  result <- pac_parse_namespace(namespace_lines, enc)

  structure(result, package = pac, version = version)
}

pac_readnamespace_raw <- function(pac, version, repos = "https://cran.rstudio.com/") {
  ee <- tempfile()

  d_url <- sprintf(
    "https://raw.githubusercontent.com/cran/%s/%s/NAMESPACE",
    pac,
    version
  )
  tt <- try(
    {
      suppressWarnings(utils::download.file(d_url,
        destfile = ee,
        quiet = TRUE
      ))
    },
    silent = TRUE
  )
  if (inherits(tt, "try-error")) {
    result <- cran_archive_file(pac, version, repos, "NAMESPACE")
  } else {
    result <- readLines(ee, warn = FALSE)
    unlink(ee)
  }

  structure(result, package = pac, version = version)
}

pac_readnamespace <- memoise::memoise(pac_readnamespace_raw, cache = cachem::cache_mem(max_age = 30 * 60))

pac_parse_namespace <- function(lines, enc) {
  directives <- if (!is.na(enc) && !Sys.getlocale("LC_CTYPE") %in%
    c("C", "POSIX")) {
    tmp <- iconv(lines, from = enc, to = "")
    bad <- which(is.na(tmp))
    comm <- grep("^[[:space:]]*#", lines[bad],
      invert = TRUE,
      useBytes = TRUE
    )
    if (length(bad[comm])) {
      stop("unable to re-encode some lines in NAMESPACE file")
    }
    tmp <- iconv(lines, from = enc, to = "", sub = "byte")
    con <- textConnection(tmp)
    on.exit(close(con))
    parse(con, keep.source = FALSE, srcfile = NULL)
  }

  exports <- character()
  exportPatterns <- character()
  exportClasses <- character()
  exportClassPatterns <- character()
  exportMethods <- character()
  imports <- list()
  importMethods <- list()
  importClasses <- list()
  dynlibs <- character()
  nS3methods <- 1000L
  S3methods <- matrix(NA_character_, nS3methods, 4L)
  nS3 <- 0L

  parseDirective <- function(e) {
    asChar <- function(cc) {
      r <- as.character(cc)
      if (any(r == "")) {
        stop(gettextf(
          "empty name in directive '%s' in 'NAMESPACE' file",
          as.character(e[[1L]])
        ), domain = NA)
      }
      r
    }
    evalToChar <- function(cc) {
      vars <- all.vars(cc)
      names(vars) <- vars
      as.character(eval(
        eval(call("substitute", cc, as.list(vars))),
        .GlobalEnv
      ))
    }
    switch(as.character(e[[1L]]),
      `if` = if (eval(
        e[[2L]],
        .GlobalEnv
      )) {
        parseDirective(e[[3L]])
      } else if (length(e) ==
        4L) {
        parseDirective(e[[4L]])
      },
      `{` = for (ee in as.list(e[-1L])) parseDirective(ee),
      `=` = ,
      `<-` = {
        parseDirective(e[[3L]])
        if (as.character(e[[3L]][[1L]]) == "useDynLib") names(dynlibs)[length(dynlibs)] <<- asChar(e[[2L]])
      },
      export = {
        exp <- e[-1L]
        exp <- structure(asChar(exp), names = names(exp))
        exports <<- c(exports, exp)
      },
      exportPattern = {
        pat <- asChar(e[-1L])
        exportPatterns <<- c(pat, exportPatterns)
      },
      exportClassPattern = {
        pat <- asChar(e[-1L])
        exportClassPatterns <<- c(pat, exportClassPatterns)
      },
      exportClass = ,
      exportClasses = {
        exportClasses <<- c(asChar(e[-1L]), exportClasses)
      },
      exportMethods = {
        exportMethods <<- c(asChar(e[-1L]), exportMethods)
      },
      import = {
        except <- e$except
        e$except <- NULL
        pkgs <- as.list(asChar(e[-1L]))
        if (!is.null(except)) {
          pkgs <- lapply(pkgs, list, except = evalToChar(except))
        }
        imports <<- c(imports, pkgs)
      },
      importFrom = {
        imp <- e[-1L]
        ivars <- imp[-1L]
        inames <- names(ivars)
        imp <- list(asChar(imp[1L]), structure(asChar(ivars),
          names = inames
        ))
        imports <<- c(imports, list(imp))
      },
      importClassFrom = ,
      importClassesFrom = {
        imp <- asChar(e[-1L])
        pkg <- imp[[1L]]
        impClasses <- imp[-1L]
        imp <- list(asChar(pkg), asChar(impClasses))
        importClasses <<- c(importClasses, list(imp))
      },
      importMethodsFrom = {
        imp <- asChar(e[-1L])
        pkg <- imp[[1L]]
        impMethods <- imp[-1L]
        imp <- list(asChar(pkg), asChar(impMethods))
        importMethods <<- c(importMethods, list(imp))
      },
      useDynLib = {
        dyl <- as.character(e[2L])
        dynlibs <<- structure(c(dynlibs, dyl), names = c(
          names(dynlibs),
          ifelse(!is.null(names(e)) && nzchar(names(e)[2L]),
            names(e)[2L], ""
          )
        ))
        if (length(e) > 2L) {
          symNames <- as.character(e[-c(1L, 2L)])
          names(symNames) <- names(e[-c(1, 2)])
          if (length(names(symNames)) == 0L) {
            names(symNames) <- symNames
          } else if (any(w <- names(symNames) ==
            "")) {
            names(symNames)[w] <- symNames[w]
          }
          dup <- duplicated(names(symNames))
          if (any(dup)) {
            warning(gettextf(
              "duplicate symbol names %s in useDynLib(\"%s\")",
              paste(sQuote(names(symNames)[dup]), collapse = ", "),
              dyl
            ), domain = NA, call. = FALSE)
          }
          symNames <- symNames[!dup]
          fixes <- c("", "")
          idx <- match(".fixes", names(symNames))
          if (!is.na(idx)) {
            if (nzchar(symNames[idx])) {
              e <- parse(
                text = symNames[idx], keep.source = FALSE,
                srcfile = NULL
              )[[1L]]
              if (is.call(e)) val <- eval(e, .GlobalEnv) else val <- as.character(e)
              if (length(val)) fixes[seq_along(val)] <- val
            }
            symNames <- symNames[-idx]
          }
          useRegistration <- FALSE
          idx <- match(".registration", names(symNames))
          if (!is.na(idx)) {
            useRegistration <- as.logical(symNames[idx])
            symNames <- symNames[-idx]
          }
        }
      },
      S3method = {
        spec <- e[-1L]
        if (length(spec) != 2L && length(spec) != 3L) {
          stop(gettextf(
            "bad 'S3method' directive: %s",
            deparse(e)
          ), call. = FALSE, domain = NA)
        }
        nS3 <<- nS3 + 1L
        if (nS3 > nS3methods) {
          old <- S3methods
          nold <- nS3methods
          nS3methods <<- nS3methods * 2L
          new <- matrix(NA_character_, nS3methods, 4L)
          ind <- seq_len(nold)
          for (i in 1:4) new[ind, i] <- old[ind, i]
          S3methods <<- new
          rm(old, new)
        }
        if (is.call(gen <- spec[[1L]]) && identical(
          as.character(gen[[1L]]),
          "::"
        )) {
          pkg <- as.character(gen[[2L]])[1L]
          gen <- as.character(gen[[3L]])[1L]
          S3methods[nS3, c(seq_along(spec), 4L)] <<- c(
            gen,
            asChar(spec[-1L]), pkg
          )
        } else {
          S3methods[nS3, seq_along(spec)] <<- asChar(spec)
        }
      },
      stop(gettextf(
        "unknown namespace directive: %s",
        deparse(e, nlines = 1L)
      ), call. = FALSE, domain = NA)
    )
  }
  for (e in directives) parseDirective(e)
  dynlibs <- dynlibs[!duplicated(dynlibs)]
  list(
    imports = imports, exports = exports, exportPatterns = unique(exportPatterns),
    importClasses = importClasses, importMethods = importMethods,
    exportClasses = unique(exportClasses), exportMethods = unique(exportMethods),
    exportClassPatterns = unique(exportClassPatterns), dynlibs = dynlibs, S3methods = unique(S3methods[seq_len(nS3), ,
      drop = FALSE
    ])
  )
}
