\name{Logical-methods}

\alias{Logical methods}
\alias{&,db.obj,db.obj-method}
\alias{&,db.obj,logical-method}
\alias{&,logical,db.obj-method}
\alias{|,db.obj,db.obj-method}
\alias{|,db.obj,logical-method}
\alias{|,logical,db.obj-method}
\alias{!,db.obj-method}

\title{Logical operations for \code{\linkS4class{db.obj}} objects}

\description{
    These binary operators perform logical operations on
  \code{\linkS4class{db.obj}} objects
}

\usage{
## S4 method for signature 'db.obj'
! x
\S4method{&}{db.obj,db.obj}(e1, e2)
\S4method{|}{db.obj,db.obj}(e1, e2)
\S4method{&}{db.obj,logical}(e1, e2)
\S4method{|}{db.obj,logical}(e1, e2)
\S4method{&}{logical,db.obj}(e1, e2)
\S4method{|}{logical,db.obj}(e1, e2)
}

\arguments{
  \item{e1, e2}{
    logical or \code{\linkS4class{db.obj}} object. 
  }

  \item{x}{
    \code{\linkS4class{db.obj}} object. 
  }
}

\value{
  \code{\linkS4class{db.Rquery}} object, which contains the SQL query
  that computes the logical operations.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\note{
  A meaningful expression is generated only when the
  \code{.col.data_type} is "boolean", otherwise a "NULL" value is
  generated. 
}

\seealso{
\code{\linkS4class{db.Rquery}} contains a SQL query that does the
operations. 
}

\examples{
\dontrun{
## get the help for a method
help("|,db.obj,db.obj-method")

## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")

preview(x$rings[x$length > 10 & x$height < 2,])
}
}

\keyword{methods}
\keyword{math}

