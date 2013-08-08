\name{cbind2-methods}
\docType{methods}
\alias{cbind2}
\alias{cbind2,db.obj,db.obj-method}
\title{Combine two \code{db.obj} Objects by Columns}
\description{
  Combine two \code{\linkS4class{db.obj}}  objects by columns.
}

\usage{
\S4method{cbind2}{db.obj,db.obj}(x, y, ...)
}

\arguments{
  \item{x, y}{
 The signature of the method. Both arguments are \code{\linkS4class{db.obj}} objects.
}
\item{\dots}{
  Other parameters, not implemented yet.
}
}
\details{
    The main use of 'cbind2' is to be called by 'cbind()'
    *if* these are activated.  This allows 'cbind'
     to work for formally classed (aka 'S4') objects by
     providing S4 methods for these objects. These are automatically
  activated when this package is loaded.
}
\value{
  A \code{\linkS4class{db.Rquery}} object, which contains all columns of
  x and y.
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
} 

\seealso{
  \code{\link{db.array}} combines columns of a table/view into an
  array. 
}
\examples{
\dontrun{

## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- db.data.frame('abalone') # use default connection 1

fit <- madlib.lm(rings ~ . - id - sex, data = x)

## create a db.Rquery object that has two columns
z <- cbind(x$rings, predict(fit, x)) 

## plot prediction v.s. real value
plot(lookat(z, 100))
}
}

\keyword{data}
\keyword{utilities}

