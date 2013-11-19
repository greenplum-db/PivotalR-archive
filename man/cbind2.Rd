\name{cbind2-methods}
\alias{as.list,db.obj-method}
\alias{cbind2}
\alias{cbind2,db.obj,db.obj-method}
\title{Combine two \code{db.obj} Objects by Columns}
\description{
  \code{cbind2} or \code{cbind} combine two or multiple \code{\linkS4class{db.obj}} objects to form a new \code{db.obj}. And \code{as.list} does the opposite and expand a \code{db.obj} object into a list of \code{db.obj} objects with each one of them representing one column of the original \code{db.obj} object. \code{as.list} is usually used together with \code{Reduce} and \code{Map}.
}

\usage{
\S4method{cbind2}{db.obj,db.obj}(x, y)

\S4method{as.list}{db.obj}(x, array = FALSE, ...)
}

\arguments{
  \item{x, y}{
 The signature of the method. Both arguments are \code{\linkS4class{db.obj}} objects.
}

\item{array}{
  logical, default is \code{FALSE}. When it is \code{TRUE}, the array columns are also expanded and all the elements are put into the resulting list. Otherwise, an array column is treated as a single item in the result.
}

\item{\dots}{
  Not implemented yet.
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
  \code{cbind2} or \code{cbind}: A \code{\linkS4class{db.Rquery}} object, which contains all columns of
x and y.

\code{as.list}: A list of \code{\linkS4class{db.Rquery}} objects, which are the columns of \code{x}
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{db.array}} combines columns of a table/view into an
  array.

  \code{\link{array.len}} measures the length of the array in an array column.
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

## expand the db.obj
unlist(Map(function(x)
               if (col.types(x) == "text")
                   paste(lk(unique(x)), collapse="-", sep="")
               else
                   lk(mean(x))),
           as.list(x))

## sum of all columns (excluding the 2nd column)
Reduce(function(left, right) left + right, as.list(x[-2]))
}
}

\keyword{data operation}
\keyword{utility}
