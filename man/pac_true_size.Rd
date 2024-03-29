% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/size.R
\name{pac_true_size}
\alias{pac_true_size}
\title{True size of the package}
\usage{
pac_true_size(
  pac,
  fields = c("Depends", "Imports", "LinkingTo"),
  lib.loc = .libPaths(),
  exclude_joint = 0L
)
}
\arguments{
\item{pac}{\code{character} a package name.}

\item{fields}{\code{character} vector listing the types of dependencies, a subset of \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}.
Character string "all" is shorthand for that vector, character string "most" for the same vector without "Enhances", character string "strong" (default) for the first three elements of that vector.
Default: \code{c("Depends", "Imports", "LinkingTo")}}

\item{lib.loc}{\code{character} vector of search paths with local packages. Default: \code{.libPaths()}}

\item{exclude_joint}{\code{integer} exclude packages which are dependencies of at least N other packages, not count main package dependencies. Default: \code{0}}
}
\value{
\code{numeric} size in bytes, to get MB then divide by \code{10**6}.
}
\description{
True size of the package as it takes into account its all dependencies, recursively.
}
\note{
R base packages are not counted. The default value of \code{fields} should be suited for almost all scenarios.
}
\examples{
\dontrun{
# size in MB, with all its dependencies
pacs::pac_true_size("memoise") / 10**6
}
}
