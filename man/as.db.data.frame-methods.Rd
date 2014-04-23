\name{as.db.data.frame}
\alias{as.db.data.frame}
\alias{as.db.data.frame,character-method}
\alias{as.db.data.frame,data.frame-method}
\alias{as.db.data.frame,db.Rquery-method}
\alias{as.db.data.frame,db.data.frame-method}
\alias{as.db.Rview}

\title{
  Convert other objects into a \code{db.data.frame} object
}

\description{
  Methods for function \code{as.db.data.frame} in package
  \pkg{PivotalR}. When \code{x} is a file name or \code{data.frame}, the
  method puts the data into a table in the database. When \code{x} is a
  \code{\linkS4class{db.Rquery}} object, it is converted into a
  table. When \code{x} is a \code{\linkS4class{db.data.frame}} object, a
  copy of the table/view that \code{x} points to is created.
}

\usage{ \S4method{as.db.data.frame}{character}(x, table.name = NULL,
verbose = TRUE, conn.id = 1, add.row.names = FALSE, key = character(0),
distributed.by = NULL, append = FALSE, is.temp = FALSE, ...)

\S4method{as.db.data.frame}{data.frame}(x, table.name = NULL, verbose =
TRUE, conn.id = 1, add.row.names = FALSE, key = character(0),
distributed.by = NULL, append = FALSE, is.temp = FALSE, ...)

\S4method{as.db.data.frame}{db.Rquery}(x, table.name = NULL, verbose =
TRUE, is.view = FALSE, is.temp = FALSE, pivot = TRUE, distributed.by =
NULL, nrow = NULL, field.types = NULL, na.as.level = FALSE,
factor.full = rep(FALSE, length(names(x))))

\S4method{as.db.data.frame}{db.data.frame}(x, table.name = NULL, verbose
= TRUE, is.view = FALSE, is.temp = FALSE, distributed.by = NULL, nrow =
NULL, field.types = NULL)

as.db.Rview(x)
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

  For \code{as.db.Rview}, \code{x} must be a \code{db.Rquery} object.
  }

  \item{table.name}{
    A string, the name of the table to be created. The returned
    \code{db.data.frame} object is pointing to this table. When
    \code{table.name} is \code{NULL}, a random name is used, which also
    avoids the name conflicts.
  }

  \item{verbose}{
    A logical, default is \code{TRUE}, whether to print some prompt messages.
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

  \item{append}{
    A logical, default is \code{FALSE}. Whether to append the content of a
    file or data.frame to an existing table in the database.
  }

  \item{nrow}{
    An integer, default is \code{NULL}. How many rows of data extracted
    from a \code{\linkS4class{db.Rquery}} object is used to create the
    new table. \code{NULL} means using all the rows.
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

  \code{nrows}When creating table from file or data.frame, the function
  will try to infer the data type of each column using the first
  \code{nrows} rows of the data.

\code{sep} specifies the field separator, and its default is ",".

\code{eol} specifies the end-of-line delimiter, and its default is "\\n".

\code{skip} specifies number of lines to skip before reading the data,
and it defaults to 0.

\code{field.types} A list of \code{key=value} pairs, where the value is
    a string of data type. Force the new table to use the data type for
    the column \code{key}.
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

  \item{na.as.level}{
      A logical value, default is \code{FALSE}. Whether to treat \code{NA}
      value as a level in a categorical variable or just ignore it.
  }

  \item{field.types}{

    A list of \code{key=value} pairs, where the value is a string of
    data type. Force the new table to use the data type for the column
    \code{key}.
  }

  \item{factor.full}{
    A vector of logical values with the length of the column number. All \code{FALSE} by default. When the function creates dummy variables for a factor (categorical) variable, whether to create \code{n} dummies or \code{n-1} dummies, where \code{n} is the number of levels of the factor. For some regression problem, we need to create dummy variables for all the distinct values of the categorical variable.
  }
}

\value{
  A \code{db.data.frame} object. It points to a table whose name is
  given by \code{table.name} in connection \code{conn.id}.

}

\note{

  All the \code{as.db.data.frame} accept the option \code{field.types}.

}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\references{
  [1] Greenplum database, \url{http://www.greenplum.com/products/greenplum-database}
}

\seealso{
  \code{\link{db.data.frame}} creates an object pointing to a table/view
  in the database.

  \code{\link{lk}} looks at data from the table

  \code{\linkS4class{db.Rquery}} this type of object represents
  operations on an existing \code{\linkS4class{db.data.frame}} object.
}
\examples{
\dontrun{
## get the help for a method
## help("as.db.data.frame")
## help("as.db.data.frame,db.Rquery-method")

%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## preview of a table
lk(x, nrows = 10) # extract 10 rows of data

## do some operations and preview the result
y <- (x[,-2] + 1.2) * 2
lk(y, 20, FALSE)

## table abalone has a column named "id"
lk(sort(x, INDICES = x$id), 20) # the preview is ordered by "id" value

## create a copied table
## x[,] converts x from db.data.frame object to db.Rquery object
z <- as.db.data.frame(x[,])

## Force the data type, use random table name

z1 <- as.db.data.frame(x$rings, field.types = list(rings="integer"))

db.disconnect(cid, verbose = FALSE)
} }

\keyword{utility}
\keyword{ database }
\keyword{methods}
