\name{na.action}
\alias{na.omit,db.obj-method}

\title{Functions for filtering \code{NA} values in data}

\description{
  'na.omit' returns the object with incomplete cases removed.
}

\usage{
\S4method{na.omit}{db.obj}(object, vars = NULL, ...)
}

\arguments{
  \item{object}{
    A \code{\linkS4class{db.obj}} object, which wraps a data table in the connected database (\code{\linkS4class{db.data.frame}}), or some operations on a data table (\code{\linkS4class{db.Rquery}}).
  }

  \item{vars}{
    An array of strings, default is \code{NULL}. The names of the columns that the user wants to filter \code{NA} values. If it is \code{NULL}, all rows that contains \code{NULL} in any column will be filtered out.
  }

  \item{\dots}{
    Further arguments, not implemented yet.
  }
}

\value{
  A \code{\linkS4class{db.Rquery}} object, which wraps the operation that filters the \code{NA} values from the columns \code{vars} in \code{object}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{madlib.lm}}, \code{\link{madlib.glm}} for linear and logistic regressions.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

delete("abalone", conn.id = cid)
dat <- as.db.data.frame(abalone, "abalone", conn.id = cid, verbose = FALSE)

madlib.lm(rings ~ . - sex - id, data = dat, na.action = na.omit)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{utility}