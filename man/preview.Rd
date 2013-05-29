\name{preview}
\alias{preview}
\alias{preview,db.view-method}
\alias{preview,db.table-method}
\alias{preview,db.Rquery-method}

\title{
  Read the actual data stored in the table of database
}
\description{
  The function reads the actual data from the table in the database.
}
\usage{
\S4method{preview}{db.table}(x, nrows = 100)

\S4method{preview}{db.view}(x, nrows = 100, interactive = FALSE)

\S4method{preview}{db.Rquery}(x, nrows = 100, interactive = FALSE)
}

\arguments{
  \item{x}{
    A \code{db.data.frame} (includes \code{db.table} and \code{db.view})
  object, which points to a table or view in
    the database; or a \code{db.Rquery} object, which represents some
  operations on a \code{db.data.frame} object.
  }
  \item{nrows}{
    An integer, default is 100, how many rows of data 
  }
  \item{interactive}{
    A boolean, default is FALSE. If \code{x} is a view, then extracting
  data from it would actually compute the view, which might take a
  longer time, especially for large data sets. When \code{interactive}
  is TRUE, this function will ask the user whether to continue to
  extract data from the view. 
  }
  
}
\value{
  A \code{data.frame} object. Each column in the table becomes a column
  of the returned \code{data.frame}. A column of arrays is converted into
  a column of strings, see \code{\link{arraydb.to.arrayr}} for more
  details. 
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{arraydb.to.arrayr}} convert strings extracted form
  database into arrays
}
\examples{
\dontrun{

## preview of a table
x <- db.data.frame("table_in_database", conn.id = 1)
preview(x, nrows = 10) # extract 10 rows of data

## do some operations and preview the result
y <- (x[,1:2] + 1.2) * 2
preview(y, 20, FALSE)

## Suppose that the table has a column named "id"
preview(sort(x, by = "id"), 20) # the preview is ordered by "id" value
}
}

\keyword{ database }
\keyword{ utility }
