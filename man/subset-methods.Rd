\name{subset-methods}

\alias{subset,db.obj-method}

\title{Extract a subset of a table or view}
\description{
    This function extracts a subset of a \code{db.obj} which could either be
    a \code{db.table} or \code{db.view} object.
}

\usage{
\S4method{subset}{db.obj}(x, subset, select)
}

\arguments{
    \item{x}{
        A \code{db.obj} (either \code{db.table} or \code{db.view}) object from
        which to extract element(s).
    }

    \item{subset, select}{
        Indices specifying elements to extract or replace.  Indices
          are `numeric' or `character' vectors or empty (missing) or
          `NULL'.  Numeric values are coerced to integer as by
          `as.integer' (and hence truncated towards zero).
    }
}

\value{
    A \code{db.Rquery} object is returned which is a SQL query to extract the
    requested subset.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
    \code{\link{[-methods}} Operator to extract elements
}

\examples{
\dontrun{
## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame
x  <- db.data.frame("madlibtestdata.lin_ornstein")
preview(x, 10)

preview(x[1:3])
preview(subset(x, 1:3))
    }
}

\keyword{data operation}
\keyword{database}
