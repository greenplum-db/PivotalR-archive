\name{crossprod}

\alias{crossprod}
\alias{crossprod,db.obj-method}

\title{
  Read the actual data stored in a table of database.
}
\description{
  The function reads the actual data from a table in the database, it
  can also preview the actual result of some operations on a
  table. \code{lookat} is actually the same as \code{preview} with
  \code{interactive} always being \code{FALSE}.
}

\usage{
\S4method{crossprod}{db.obj}(x, y = NULL)
}

\arguments{
  \item{x}{
    The signature of the method. 
    A \code{db.data.frame} (includes \code{db.table} and \code{db.view})
  object, which points to a table or view in
    the database; or a \code{db.Rquery} object, which represents some
  operations on a \code{db.data.frame} object.
  }
  \item{y}{
    An integer, default is 100, how many rows of data. If it is
  \code{NULL} or \code{"all"}, then all data in the table will be send
  into R. Be careful, you do not want to do this if the data table is
  very large.
  }
 }

\details{
  When the signature \code{x} is a \code{db.data.frame} object, this
  function reads the data in a table or view in the connected database.

  When the signature \code{x} is a \code{db.Rquery} object, this
  function reads the result of some operations on a \code{db.data.frame}
  object. 
}

\value{
  A \code{data.frame} object. Each column in the table becomes a column
  of the returned \code{data.frame}. A column of arrays is converted into
  a column of strings, see \code{\link{arraydb.to.arrayr}} for more
  details. 
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{arraydb.to.arrayr}} convert strings extracted form
  database into arrays.
}
\examples{
\dontrun{
## get the help for a method
help("crossprod,db.obj-method")

## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")

## preview of a table
preview(x, nrows = 10) # extract 10 rows of data

## do some operations and preview the result
y <- (x[,1:2] + 1.2) * 2
preview(y, 20, FALSE)

## table abalone has a column named "id"
preview(sort(x, by = "id"), 20) # the preview is ordered by "id" value
}
}

\keyword{methods}
\keyword{database}
\keyword{utility}
