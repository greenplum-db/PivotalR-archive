\name{Func-methods}

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

\title{Functions that take \code{\linkS4class{db.obj}} objects as the
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
}

\arguments{
  \item{x}{
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
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
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
help("+,db.obj,db.obj-method")

## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")

x$rings <- exp(x$rings) # change the values

x$area <- log(x$length * x$height) # add a new column

preview(x$area, 10) # view the actual values computed in database

fit <- madlib.lm(rings ~ area, data = x)
}
}

\keyword{methods}
\keyword{math}