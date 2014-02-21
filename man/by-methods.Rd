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
    A function, which will be applied onto each group of the data. The result of \code{FUN} can be of \code{\linkS4class{db.obj}} type or any other data types that R supports.
  }

  \item{\dots}{
    Extra arguments passed to \code{FUN}, currently not implemented.
  }

  \item{simplify}{
    Not implemented yet.
  }
}

\value{
  The type of the returned value depends on the return type of   \code{FUN}.

If the return type of \code{FUN} is a \code{\linkS4class{db.obj}} object, then this function returns
  a \code{db.Rquery} object, which is actually the SQL query that does
  the "GROUP BY". It computes the group-by values. The result can be
  viewed using \code{\link{lk}} or \code{\link{lookat}}.

  If the return type of \code{FUN} is not a \code{\linkS4class{db.obj}} object, then this function returns a list, which contains a number of sub-lists. Each sub-list contains two items: (1) \code{index}, an array of strings, a set of distinct values of the \code{INDICES} converted to string; and (2) \code{result}, the result produced by \code{FUN} applying onto the group of data that has the set of distinct values. The total number of sub-lists is equal to the total number of groups of data partitioned by \code{INDICES}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{Aggregate functions}} lists all the supported aggregate
  functions.

  \code{\link{lk}} or \code{\link{lookat}} can display the actual result of this function.
}

\examples{
\dontrun{
## help("by,db.obj-method") # display this doc
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## mean values for each column
lk(by(x, x$sex, mean))

## No need to compute the mean of id and sex
lk(by(x[,-c(1,2)], x$sex, mean))
lk(by(x[,-c(1,2)], x[,2], mean)) # the same
lk(by(x[,-c(1,2)], x[,"sex"], mean)) # the same

## The return type of FUN is not db.obj
dat <- x

## Fit linear model to each group of data
by(dat, dat$sex, function(x) madlib.lm(rings ~ . - id - sex, data = x))

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{data operation}