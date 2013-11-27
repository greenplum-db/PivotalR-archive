\name{preview}
\alias{lk}
\alias{lookat}
\alias{preview}
\alias{preview,character-method}
\alias{preview,db.view-method}
\alias{preview,db.table-method}
\alias{preview,db.Rquery-method}
\alias{preview,db.Rcrossprod-method}
\alias{as.data.frame.db.table}
\alias{as.data.frame.db.view}
\alias{as.data.frame.db.Rquery}

\title{
  Read the actual data stored in a table of database.
}
\description{
  These functions read the actual data from a database table or operation, returning a \code{\link{data.frame}} or other object as appropriate. \code{lookat} and \code{lk} are actually the same as \code{preview} with \code{interactive} always being \code{FALSE}.
}

\usage{
preview(x, ..., drop = TRUE)

\S4method{preview}{db.table}(x, nrows = 100, array = TRUE)

\S4method{preview}{db.view}(x, nrows = 100, interactive = FALSE,
array = TRUE)

\S4method{preview}{db.Rquery}(x, nrows = 100, interactive = FALSE,
array = TRUE)

\S4method{preview}{db.Rcrossprod}(x, nrows = 100, interactive = FALSE)

\S4method{preview}{character}(x, conn.id = 1, nrows = 100, array = TRUE)

lookat(x, nrows = 100, array = TRUE, conn.id = 1, drop = TRUE)

lk(x, nrows = 100, array = TRUE, conn.id = 1, drop = TRUE)

\method{as.data.frame}{db.table}(x, row.names = NULL, optional = FALSE,
     nrows = NULL, stringsAsFactors = default.stringsAsFactors(), array
     = TRUE, ...)

\method{as.data.frame}{db.view}(x,row.names=NULL, optional=FALSE, nrows
    = NULL, stringsAsFactors = default.stringsAsFactors(), array = TRUE,
    ...)

\method{as.data.frame}{db.Rquery}(x, row.names = NULL, optional =
    FALSE, nrows = NULL, stringsAsFactors = default.stringsAsFactors(),
    array = TRUE, ...)  }

\arguments{
  \item{x}{
    A \code{\linkS4class{db.data.frame}} (includes
    \code{\linkS4class{db.table}} and \code{\linkS4class{db.view}})
    object, which points to a table or view in
    the database; or a \code{\linkS4class{db.Rquery}} object, which
    represents some 
    operations on a \code{\linkS4class{db.data.frame}} object. If \code{x}
    is a string, which means a table name, this function directly reads
    data from the table without having to wrap it with a
    \code{db.data.frame} class object.
  }

  \item{conn.id}{
    An integer, the ID of the connection where the table resides.
  }

  \item{nrows}{
    An integer, how many rows of data to retrieve. If it is
  \code{NULL} or \code{"all"} or a non-positive value, then all data in the table will be send
  into R. Be careful, you do not want to do this if the data table is
  very large.
  }
  \item{interactive}{
    A boolean, default is \code{FALSE}. If \code{x} is a view, then
  extracting 
    data from it would actually compute the view, which might take a
    longer time, especially for large data sets. When \code{interactive}
    is TRUE, this function will ask the user whether to continue to
    extract data from the view. 
  }
  \item{array}{
    Logical, default is \code{TRUE}. This decides how to parse columns
  that have array as their elements. When \code{TRUE}, each
  element in the array is extracted and put into a new
  column. Otherwise, the array is read in as a string.
  }
  \item{stringsAsFactors}{
    Logical, whether character variables should be converted to factors.
  }
  \item{drop}{Whether to coerce single-column tables to a vector of the
    appropriate class.}
  \item{row.names,optional,...}{
    For compatibility with the \code{as.data.frame} generic; not used.
  }
}

\details{
When \code{x} is a \code{\linkS4class{db.data.frame}} object, this 
  function reads the data in a table or view in the connected database.

  When \code{x} is a \code{\linkS4class{db.Rquery}} object, this 
  function reads the result of some operations on a
  \code{\linkS4class{db.data.frame}} 
  object. 

  When \code{x} is a \code{\linkS4class{db.Rcrossprod}}
  object, this function output a matrix to R. If the matrix is symmetric,
  it is returned as \code{\linkS4class{dspMatrix}}. Otherwise, it is
  returned as \code{\linkS4class{dgeMatrix}}. If there are multiple
  matrices in \code{x}, a list is returned and each element of the list
  is a matrix.

  The \code{as.data.frame} method calls \code{lookat} with \code{nrows = NULL} to perform the conversion to a data frame. In this case \code{drop} is set to \code{FALSE}, ie the result will always be a data frame.
}

\value{
  For \code{db.data.frame} and \code{db.Rquery} objects, a data frame. Each column in the table becomes a column
  of the returned \code{data.frame}. A column of arrays is converted into
  a column of strings, see \code{\link{arraydb.to.arrayr}} for more
  details. Single-column tables created with \code{preview}, \code{lookat} or \code{lk} will be coerced to a vector if \code{drop == TRUE}.
  
  For \code{db.Rcrossprod} objects, a matrix or list of matrix objects as appropriate (see above).
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Hong Ooi, Pivotal Inc. \email{hooi@gopivotal.com} wrote the
  \code{as.data.frame} methods.
  
  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{arraydb.to.arrayr}} convert strings extracted form
  database into arrays.
}
\examples{
\dontrun{
## get the help for a method
help("preview,db.Rquery-method")

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

## use as.data.frame
as.data.frame(x, 10) } }

\keyword{methods}
\keyword{data operation}
\keyword{database}
\keyword{utility}
