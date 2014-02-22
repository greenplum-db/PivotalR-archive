\name{Func-methods}
\alias{Math Functions}
\alias{exp}
\alias{abs}
\alias{log}
\alias{log10}
\alias{sign}
\alias{sqrt}
\alias{factorial}
\alias{exp,db.obj-method}
\alias{abs,db.obj-method}
\alias{log,db.obj-method}
\alias{log10,db.obj-method}
\alias{sign,db.obj-method}
\alias{sqrt,db.obj-method}
\alias{factorial,db.obj-method}
\alias{sin,db.obj-method}
\alias{cos,db.obj-method}
\alias{asin,db.obj-method}
\alias{acos,db.obj-method}
\alias{atan,db.obj-method}
\alias{tan,db.obj-method}
\alias{atan2,db.obj,db.obj-method}
\alias{atan2,db.obj,numeric-method}
\alias{atan2,numeric,db.obj-method}

\title{Mathematical functions that take \code{\linkS4class{db.obj}} objects as the
  argument}

\description{
 Functions that apply onto
  \code{\linkS4class{db.obj}} objects
}

\usage{
\S4method{exp}{db.obj}(x)
\S4method{abs}{db.obj}(x)
\S4method{log}{db.obj}(x, ...)
\S4method{log10}{db.obj}(x)
\S4method{sign}{db.obj}(x)
\S4method{sqrt}{db.obj}(x)
\S4method{factorial}{db.obj}(x)
\S4method{sin}{db.obj}(x)
\S4method{cos}{db.obj}(x)
\S4method{tan}{db.obj}(x)
\S4method{asin}{db.obj}(x)
\S4method{acos}{db.obj}(x)
\S4method{atan}{db.obj}(x)
\S4method{atan2}{db.obj,db.obj}(y, x)
\S4method{atan2}{db.obj,numeric}(y, x)
\S4method{atan2}{numeric,db.obj}(y, x)
}

\arguments{
  \item{x,y}{
    \code{\linkS4class{db.obj}} object. The function applies to each
  column of the \code{\linkS4class{db.obj}} object. If a column is an
  array, then the function applies onto each element of the array. If
  the data type of the column makes no sense to be used in the function,
  then a null value is returned.
  }

  \item{\dots}{
    Extra parameters. Not implemented.
  }
}

\value{
  \code{\linkS4class{db.Rquery}} object, which contains the SQL query
  that computes the operations.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\note{
  A meaningful expression is generated only when the
  \code{.col.data_type} is "numeric", otherwise a "NULL" value is
  generated.
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
cid <- db.connect(port = .port, dbname = .dbname)

## create a table from the example data.frame "abalone"
delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid)

x$rings <- exp(x$rings) # change the values

x$area <- log((x$length + 1) * (x$height + 1)) # add a new column

lk(x$area, 10) # view the actual values computed in database

fit <- madlib.lm(rings ~ area, data = x)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{math}