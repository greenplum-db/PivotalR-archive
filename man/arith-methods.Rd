\name{Arith-methods}

\alias{Arith methods}
\alias{+,db.obj,db.obj-method}
\alias{+,db.obj,numeric-method}
\alias{+,numeric,db.obj-method}
\alias{-,db.obj,db.obj-method}
\alias{-,db.obj,numeric-method}
\alias{-,numeric,db.obj-method}
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
\S4method{*}{db.obj,db.obj}(e1, e2)
\S4method{/}{db.obj,db.obj}(e1, e2)
\S4method{\%\%}{db.obj,db.obj}(e1, e2)
\S4method{\%/\%}{db.obj,db.obj}(e1, e2)
\S4method{^}{db.obj,db.obj}(e1, e2)
\S4method{+}{numeric,db.obj}(e1, e2)
\S4method{-}{numeric,db.obj}(e1, e2)
\S4method{*}{numeric,db.obj}(e1, e2)
\S4method{/}{numeric,db.obj}(e1, e2)
\S4method{\%\%}{numeric,db.obj}(e1, e2)
\S4method{\%/\%}{numeric,db.obj}(e1, e2)
\S4method{^}{numeric,db.obj}(e1, e2)
\S4method{+}{db.obj,numeric}(e1, e2)
\S4method{-}{db.obj,numeric}(e1, e2)
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

x$rings <- (x$rings + 2.3) * 3 # change the values

x$area <- x$length * x$height # add a new column

preview(x$area, 10) # view the actual values computed in database

fit <- madlib.lm(rings ~ area, data = x)
}
}

\keyword{math}