\name{unique-methods}

\alias{unique,db.obj-method}

\title{The Unique of an object}
\description{
    This function gives the unique valuesof a \code{db.obj}, which are
    the column 
    unique of a \code{db.table} or \code{db.view}. The unique values are
    returned as arrays in one row. But for a column of array values, the
    unique values are returned as multiple rows. 
}

\usage{
\S4method{unique}{db.obj}(x, incomparables = FALSE, ...)
}

\arguments{
    \item{x}{
        A \code{db.obj} object, which the column unique are required.
      }

      \item{incomparables}{
        Not implemented.
      }

      \item{\dots}{
        Not implemented.
      }
}

\value{
    An \code{\linkS4class{db.Rquery}} with the arrays of the column
    unique values. Each 
    column will have one array which contains all the distinct values of
    that column. However, if the column under consideration is already
    an array column, then multiple rows will be returned and each row
    contains one distinct value of the array.
}

\note{This function applies only onto \code{\linkS4class{db.obj}} with
  one column. If you want to put the unique values from multiple columns
  together, you have to use \code{\link{db.array}}}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\linkS4class{db.obj}}, \code{\linkS4class{db.data.frame}},
  \code{\linkS4class{db.table}}, \code{\linkS4class{db.view}},
  \code{\linkS4class{db.Rquery}} are the class hierarchy structure of
  this package.
}

\examples{
    \dontrun{
## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")
## preview of a table
preview(x, nrows = 10) # extract 10 rows of data

## get unique of all columns
unique(x$sex)
}
}

\keyword{methods}
\keyword{database}