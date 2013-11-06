\name{sort}

\alias{sort,db.obj-method}

\title{Sort a table or view by a set of columns}

\description{
  This function is used to sort a table of view in the database.
}

\usage{
\S4method{sort}{db.obj}(x, decreasing = FALSE, INDICES, ...)
}

\arguments{
    \item{x}{
      The signature of the method.
      A \code{db.obj} (includes \code{db.table} and \code{db.view})
    object, which points to a table or view in the database.
  }

  \item{decreasing}{
      A logical, with default value as FALSE.  Should the sort be increasing or decreasing?
  }

  \item{INDICES}{
    A list of \code{db.Rquery} objects. Each of the list element selects one or
    multiple columns of \code{x}. \code{NULL} to order by random().
  }

  \item{\dots}{
      Further arguments passed to or from other methods.
      This is currently not implemented.
  }
}

\value{
  A \code{db.Rquery} object. It is the query object used to sort the \code{db.obj} in the
  database.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{by}} has similar syntax to this function.
  
  \code{\link{preview}} to view portion of the data table
}

\examples{
\dontrun{
# Suppose that a valid connection with ID 1 exists
x  <- db.data.frame("madlibtestdata.lin_ornstein")
preview(x, 10)

y  <- sort(x, decreasing = FALSE, list(x$nation, x$sector) )
# get the SQL query to be run
content(y)
# get the sorted output
preview(y)
}
}

\keyword{database}
\keyword{methods}
\keyword{utility}
