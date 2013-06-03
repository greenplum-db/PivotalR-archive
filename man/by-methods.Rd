\name{by}
\alias{by,db.obj-method}
\title{ Apply a Function to a \code{db.data.frame} Split by column(s) }
\description{
  \code{by} is equivalent to "group by" in SQL language. It groups the
  data according the value(s) of one or multiple columns, and then apply
  an aggregate function onto each group of the data.
}

\usage{
\S4method{by}{db.obj}(data, INDICES, FUN, ..., simplify = TRUE)
}

\arguments{
  \item{data}{
    A \code{db.obj} object. It represents a table/view in the database
  if it is an \code{db.data.frame} object,
  or a series of operations applied on an existing \code{db.data.frame}
  object if it is a \code{db.Rquery} object.
  }

  \item{INDICES}{
    A list of \code{db.Rquery} objects. Each of
    the list element selects one or multiple columns of \code{data}. When the value is
    \code{NULL}, no grouping of data is done, and the aggregate function
    \code{FUN} will be applied onto 
    all the data.
  }

  \item{FUN}{
    An aggregate function, which will be applied onto each group of the data.
  }

  \item{\dots}{
    Extra arguments passed to \code{FUN}, currently not implemented.
  }

  \item{simplify}{
    Not implemented yet.
  }
}

\value{
  A \code{db.Rquery} object, which is actually the SQL query that does
  the "GROUP BY".  
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{Aggregate functions}} lists all the supported aggregate
  functions.

  \code{\link{preview}} can display the actual result of this function.
}

\examples{
\dontrun{
help("by,db.obj-method") # display this doc

## Suppose a valid database connection 1 has been created by db.connect

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone") # use default connection 1

## mean values for each column
preview(by(x, x$sex, mean))

## No need to compute the mean of id and sex
preview(by(x[,-c(1,2)], x$sex, mean))
preview(by(x[,-c(1,2)], x[,2], mean)) # the same
preview(by(x[,-c(1,2)], x[,"sex"], mean)) # the same
}
}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
