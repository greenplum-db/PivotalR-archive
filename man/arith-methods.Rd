\name{Arith-methods}

\alias{Arith methods}
\alias{+,db.obj,db.obj-method}
\alias{+,db.obj,numeric-method}
\alias{+,numeric,db.obj-method}
\alias{+,db.obj,character-method}
\alias{+,character,db.obj-method}
\alias{-,db.obj,ANY-method}
\alias{-,db.obj,db.obj-method}
\alias{-,db.obj,numeric-method}
\alias{-,numeric,db.obj-method}
\alias{-,db.obj,character-method}
\alias{-,character,db.obj-method}
\alias{*,db.obj,db.obj-method}
\alias{*,db.obj,numeric-method}
\alias{*,numeric,db.obj-method}
\alias{/,db.obj,db.obj-method}
\alias{/,db.obj,numeric-method}
\alias{/,numeric,db.obj-method}
\alias{\%/\%,db.obj,db.obj-method}
\alias{\%/\%,db.obj,numeric-method}
\alias{\%/\%,numeric,db.obj-method}
\alias{\%\%,db.obj,db.obj-method}
\alias{\%\%,db.obj,numeric-method}
\alias{\%\%,numeric,db.obj-method}
\alias{^,db.obj,db.obj-method}
\alias{^,db.obj,numeric-method}
\alias{^,numeric,db.obj-method}

\title{Arithmetic Operators for \code{\linkS4class{db.obj}} objects}

\description{
  These binary operators perform arithmetic on
  \code{\linkS4class{db.obj}} objects
}

\usage{
\S4method{+}{db.obj,db.obj}(e1, e2)
\S4method{-}{db.obj,db.obj}(e1, e2)
\S4method{-}{db.obj,ANY}(e1, e2)
\S4method{*}{db.obj,db.obj}(e1, e2)
\S4method{/}{db.obj,db.obj}(e1, e2)
\S4method{\%\%}{db.obj,db.obj}(e1, e2)
\S4method{\%/\%}{db.obj,db.obj}(e1, e2)
\S4method{^}{db.obj,db.obj}(e1, e2)
\S4method{+}{numeric,db.obj}(e1, e2)
\S4method{+}{character,db.obj}(e1, e2)
\S4method{-}{numeric,db.obj}(e1, e2)
\S4method{-}{character,db.obj}(e1, e2)
\S4method{*}{numeric,db.obj}(e1, e2)
\S4method{/}{numeric,db.obj}(e1, e2)
\S4method{\%\%}{numeric,db.obj}(e1, e2)
\S4method{\%/\%}{numeric,db.obj}(e1, e2)
\S4method{^}{numeric,db.obj}(e1, e2)
\S4method{+}{db.obj,numeric}(e1, e2)
\S4method{+}{db.obj,character}(e1, e2)
\S4method{-}{db.obj,numeric}(e1, e2)
\S4method{-}{db.obj,character}(e1, e2)
\S4method{*}{db.obj,numeric}(e1, e2)
\S4method{/}{db.obj,numeric}(e1, e2)
\S4method{\%\%}{db.obj,numeric}(e1, e2)
\S4method{\%/\%}{db.obj,numeric}(e1, e2)
\S4method{^}{db.obj,numeric}(e1, e2)
}

\arguments{
  \item{e1, e2}{
    numeric or \code{\linkS4class{db.obj}} object.
  }
}

\value{
  \code{\linkS4class{db.Rquery}} object, which contains the SQL query
  that computes the arithmetic operations.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\note{
  A meaningful expression is generated only when the
  \code{.col.data_type} is "numeric", otherwise a "NULL" value is
  generated.

  \code{"-"} and \code{"+"} support computing the arithmetic computations between dates, timestamps, times etc.
}

\seealso{
\code{\linkS4class{db.Rquery}} contains a SQL query that does the
operations.
}

\examples{
\dontrun{
## get the help for a method
## help("+,db.obj,db.obj-method")

%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## ----------------------------------------------------------------------

## create a table from the example data.frame "abalone"
delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid, verbose = FALSE)

x$rings <- (x$rings + 2.3) * 3 # change the values

x$area <- x$length * x$height # add a new column

lk(x$area, 10) # view the actual values computed in database

fit <- madlib.lm(rings ~ area, data = x)

## ----------------------------------------------------------------------

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{math}
