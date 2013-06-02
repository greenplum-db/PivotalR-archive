\name{[[-methods}

\alias{[[,db.obj-method}

\title{Extract an element (column) of a database object}

\description{
    This function extracts an element of \code{db.obj} (either a
    \code{db.table}, \code{db.view} object or a \code{db.Rquery}).
}

\usage{
\S4method{[[}{db.obj}(x, i, j, ...)
}

\arguments{
    \item{x}{
        A \code{db.obj} (either \code{db.table}, \code{db.view},
        or \code{db.Rquery}) from which to extract element(s).
    }

    \item{i, j, ...}{
        Index specifying element to extract or replace.  Index
          is `numeric' or `character' or empty (missing) or
          `NULL'.  Numeric value is coerced to integer as by
          `as.integer' (and hence truncated towards zero).
    }
}

\value{
    A \code{db.Rquery} object is returned, a SQL query to extract the
    requested subset.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
}

\seealso{
    \code{\link{subset,db.obj-method}} Operator to extract elements
    \code{\link{[-methods}} Operator to extract elements
}

\examples{
\dontrun{
## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame
x  <- db.data.frame("madlibtestdata.lin_ornstein", key='assets')
names(x)

preview(x[[1]])
content(x[[1, 'sector']])
    }
}

\keyword{manip}
