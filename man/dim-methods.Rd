\name{dim-methods}

\alias{dim,db.obj-method}

\title{Dimension of a table}

\description{
  Display the dimension of the table that a \code{db.table} object
  points to.
}

\usage{
\S4method{dim}{db.obj}(x)
}

\arguments{
  \item{x}{
    A \code{db.obj}. Only for \code{db.table} object, this function
  gives the dimension of table that \code{x} points to. For
  \code{db.view} and \code{db.Rquery} objects, an error message is raised.
  }
}

\value{
  A two-integer array, where the first integer is the number of rows and the
  second integer is the number of columns.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{db.obj-class}}, \code{\link{db.data.frame-class}},
  \code{\link{db.table-class}}, \code{\link{db.view-class}},
  \code{\link{db.Rquery-class}} are the class hierarchy structure of
  this package.
}

\examples{
    \dontrun{
## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")
## preview of a table
preview(x, nrows = 10) # extract 10 rows of data

## get names of all columns
dim(x)
    }
}

\keyword{methods}

