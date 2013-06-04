\name{as.db.data.frame}
\alias{as.db.data.frame}
\alias{as.db.data.frame,character-method}
\alias{as.db.data.frame,data.frame-method}
\alias{as.db.data.frame,db.Rquery-method}

\title{
  Convert other objects into a \code{db.data.frame} object
}

\description{
  Methods for function \code{as.db.data.frame} in package \pkg{PivotalR}
}

\usage{
\S4method{as.db.data.frame}{character}(x, table.name, verbose = FALSE,
conn.id = 1, add.row.names = FALSE, key = character(0), distributed.by =
NULL, is.temp = FALSE, ...)

\S4method{as.db.data.frame}{data.frame}(x, table.name, verbose = FALSE,
conn.id = 1, add.row.names = FALSE, key = character(0), distributed.by =
NULL, is.temp = FALSE, ...)

\S4method{as.db.data.frame}{db.Rquery}(x, table.name, verbose = FALSE,
is.view = FALSE, is.temp = FALSE, pivot = TRUE, distributed.by = NULL)
}

\arguments{
  \item{x}{
    The signature of this method.

    When it is of type \code{character},
    it should be a file name.

    When it is of type \code{data.frame}, it is
  the \code{data.frame} that already exists in the current R
  session.

  When it is of type \code{db.Rquery}, it represents a series of
  operations on a existing \code{db.data.frame} object. See
  \code{\linkS4class{db.Rquery}} for more.
  }

  \item{table.name}{
    A string, the name of the table to be created. The returned
    \code{db.data.frame} object is pointing to this table.
  }

  \item{verbose}{
    A logical, default is \code{FALSE}, whether to print some prompt messages.
  }

  \item{conn.id}{
    An integer, default is 1. The ID of the connection. See
    \code{\link{db.list}} for more information.
  }

  \item{add.row.names}{
    A logical, default is \code{FALSE}, whether to add a column named
    "row.names" is added to the newly created table as the first column,
    which is just the row number of the original \code{data.frame} or file.
  }

  \item{key}{
    A string, default is \code{character(0)}. The primary key column
    name. When it is not \code{character(0)}, a primary key is created
    for this column.
  }

  \item{distributed.by}{
    A string, default is \code{NULL}. It is a column name or multiple
    column names separated by comma. When creating tables in a Greenplum
    database [1], the user can choose to specify whether he want to
    distributed the table onto multiple segments according the values
    of some columns. When this parameter is \code{NULL}, the data is
    distributed randomly, and when this parameter is an empty string
    code{""}, Greenplum database automatically chooses a column and
    distribute the data according to that column.
  }

  \item{is.temp}{
    A logical, default is \code{FALSE}, whether the created table/view should
    be a temporary table/view.
  }

  \item{\dots}{
    Extra parameters used to create the table inside the database. We
    support the following parameters: \code{header = FALSE}, \code{nrows
    = 50}, \code{sep = ","}, \code{eol = "\n"}, \code{skip = 0}.

  \code{header} is a logical indicating whether the first data line (but
  see skip) has a header or not. If missing, it value is determined
  following \code{read.table} convention, namely, it is set to TRUE if
  and only if the first row has one fewer field that the number of
  columns.

  \code{nrows}{When creating table from file or data.frame, the function
  will try to infer the data type of each column using the first
  \code{nrows} rows of the data.}

\code{sep} specifies the field separator, and its default is ",".

\code{eol} specifies the end-of-line delimiter, and its default is "\\n".

\code{skip} specifies number of lines to skip before reading the data,
and it defaults to 0.
  }

  \item{is.view}{
    A logical, default is \code{FALSE}, whether to create a view instead
    of a table.
  }

  \item{pivot}{
    A logical, default is \code{TRUE}, whether to create dummy columns
    for a column that has been denoted as "factor". See
    \code{\link{as.factor}} for more details.
  }
}

\value{
  A \code{db.data.frame} object. It points to a table whose name is
  given by \code{table.name} in connection \code{conn.id}. 

}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\references{
  [1] Greenplum database, \url{http://www.greenplum.com/products/greenplum-database}
}

\seealso{
  \code{\link{db.data.frame}} creates an object pointing to a table/view
  in the database.

  \code{\link{preview}} reads data from the table

  \code{\linkS4class{db.Rquery}} this type of object represents
  operations on an existing \code{\linkS4class{db.data.frame}} object.
}
\examples{
\dontrun{
## get the help for a method
help("as.db.data.frame")
help("as.db.data.frame,db.Rquery-method")

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

## create a copied table
## x[,] converts x from db.data.frame object to db.Rquery object
z <- as.db.data.frame(x[,], "abalone1") 
}
}

\keyword{methods}
\keyword{ database }
