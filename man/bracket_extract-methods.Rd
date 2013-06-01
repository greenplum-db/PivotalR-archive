\name{[-methods}
\docType{methods}
\alias{[-methods}
\alias{[,ANY-method}
\alias{[,db.obj-method}
\alias{[,nonStructure-method}

\title{Extract a subset of a table or view}

\description{
    This function extracts a subset of a \code{db.obj}(a \code{db.table} or
    \code{db.view} object, or a \code{db.Rquery}).
}

\usage{
    \S4method{[}{db.obj}(x, i, j, ..., drop = TRUE)
    \S4method{[}{ANY}(x, i, j, ..., drop = TRUE)
    \S4method{[}{nonStructure}(x, i, j, ..., drop = TRUE)
}

\arguments{
    \item{x}{
        A \code{db.obj} (either \code{db.table}, \code{db.view},
        or \code{db.Rquery}) from which to extract element(s).
    }

    \item{i, j, ...}{
        Indices specifying elements to extract or replace.  Indices
          are `numeric' or `character' vectors or empty (missing) or
          `NULL'.  Numeric values are coerced to integer as by
          `as.integer' (and hence truncated towards zero).
    }

    \item{drop}{
      For matrices and arrays.  If `TRUE' the result is coerced to
          the lowest possible dimension (see the examples).
      This is currently not implemented.
    }
}

\value{
    A \code{db.Rquery} object is returned, a SQL query to extract the
    requested subset.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
    \code{\link{subset-methods}} Operator to extract elements
}

\examples{
\dontrun{
## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame
x  <- db.data.frame("madlibtestdata.lin_ornstein")
preview(x, 10)

preview(x[1:3])
preview(x[c('assets', 'nation')])
    }
}

\keyword{manip}
