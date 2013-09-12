\name{Compare-methods}

\alias{Compare methods}
\alias{>,character,db.obj-method}
\alias{>,db.obj,character-method}
\alias{>,db.obj,db.obj-method}
\alias{>,db.obj,numeric-method}
\alias{>,numeric,db.obj-method}
\alias{<,character,db.obj-method}
\alias{<,db.obj,character-method}
\alias{<,db.obj,db.obj-method}
\alias{<,db.obj,numeric-method}
\alias{<,numeric,db.obj-method}
\alias{>=,character,db.obj-method}
\alias{>=,db.obj,character-method}
\alias{>=,db.obj,db.obj-method}
\alias{>=,db.obj,numeric-method}
\alias{>=,numeric,db.obj-method}
\alias{<=,character,db.obj-method}
\alias{<=,db.obj,character-method}
\alias{<=,db.obj,db.obj-method}
\alias{<=,db.obj,numeric-method}
\alias{<=,numeric,db.obj-method}
\alias{==,character,db.obj-method}
\alias{==,db.obj,character-method}
\alias{==,db.obj,db.obj-method}
\alias{==,db.obj,numeric-method}
\alias{==,numeric,db.obj-method}
\alias{!=,character,db.obj-method}
\alias{!=,db.obj,character-method}
\alias{!=,db.obj,db.obj-method}
\alias{!=,db.obj,numeric-method}
\alias{!=,numeric,db.obj-method}

\title{Comparison Operators for \code{\linkS4class{db.obj}} objects}

\description{
  These binary operators perform comparison on
  \code{\linkS4class{db.obj}} objects
}

\usage{
\S4method{>}{db.obj,db.obj}(e1, e2)
\S4method{<}{db.obj,db.obj}(e1, e2)
\S4method{>=}{db.obj,db.obj}(e1, e2)
\S4method{<=}{db.obj,db.obj}(e1, e2)
\S4method{==}{db.obj,db.obj}(e1, e2)
\S4method{!=}{db.obj,db.obj}(e1, e2)
\S4method{>}{character,db.obj}(e1, e2)
\S4method{<}{character,db.obj}(e1, e2)
\S4method{>=}{character,db.obj}(e1, e2)
\S4method{<=}{character,db.obj}(e1, e2)
\S4method{==}{character,db.obj}(e1, e2)
\S4method{!=}{character,db.obj}(e1, e2)
\S4method{>}{db.obj,character}(e1, e2)
\S4method{<}{db.obj,character}(e1, e2)
\S4method{>=}{db.obj,character}(e1, e2)
\S4method{<=}{db.obj,character}(e1, e2)
\S4method{==}{db.obj,character}(e1, e2)
\S4method{!=}{db.obj,character}(e1, e2)
\S4method{>}{numeric,db.obj}(e1, e2)
\S4method{<}{numeric,db.obj}(e1, e2)
\S4method{>=}{numeric,db.obj}(e1, e2)
\S4method{<=}{numeric,db.obj}(e1, e2)
\S4method{==}{numeric,db.obj}(e1, e2)
\S4method{!=}{numeric,db.obj}(e1, e2)
\S4method{>}{db.obj,numeric}(e1, e2)
\S4method{<}{db.obj,numeric}(e1, e2)
\S4method{>=}{db.obj,numeric}(e1, e2)
\S4method{<=}{db.obj,numeric}(e1, e2)
\S4method{==}{db.obj,numeric}(e1, e2)
\S4method{!=}{db.obj,numeric}(e1, e2)
}

\arguments{
  \item{e1, e2}{
    numeric, character or \code{\linkS4class{db.obj}} object.
  }
}

\value{
  \code{\linkS4class{db.Rquery}} object, which contains the SQL query
  that computes the comparison operations.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\note{
  A meaningful expression is generated only when the
  \code{.col.data_type} is "character" or "numeric", otherwise a "NULL"
  value is 
  generated. 
}

\seealso{
\code{\linkS4class{db.Rquery}} contains a SQL query that does the
operations. 
}

\examples{
\dontrun{
## get the help for a method
help(">,db.obj,db.obj-method")

## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")

preview(x[x$length > 10,])
}
}

\keyword{methods}
\keyword{math}
