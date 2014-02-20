\name{is.factor-methods}

\alias{is.factor,db.obj-method}

\title{ Detect whether a \code{\linkS4class{db.obj}} object is a
  categorical object }

\description{
  This function detects whether a \code{\linkS4class{db.obj}} object is a
  categorical object.
}

\usage{
\S4method{is.factor}{db.obj}(x)
}

\arguments{
  \item{x}{
    A \code{\linkS4class{db.obj}} object.
  }
}

\value{
  A logical value. When all columns of \code{\linkS4class{db.obj}} are
  categorical variables, this function returns \code{TRUE}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{as.factor,db.obj-method}} converts a column
  \code{\linkS4class{db.obj}} of into categorical variables.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## set sex to be a categorical variable
x$sex <- as.factor(x$sex)

is.factor(x$sex)

is.factor(x)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{math}
