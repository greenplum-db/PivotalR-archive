\name{subset-methods}

\alias{subset,db.obj-method}

\title{Extract a subset of a table or view}
\description{
    This function extracts a subset of a \code{db.obj} which could either be
    a \code{db.table} or \code{db.view} object.
}

\usage{
\S4method{subset}{db.obj}(x, subset, select)
}

\arguments{
    \item{x}{
        A \code{db.obj} (either \code{db.table} or \code{db.view}) object from
        which to extract element(s).
    }

    \item{subset, select}{
        Indices specifying elements to extract or replace.  Indices
          are `numeric' or `character' vectors or empty (missing) or
          `NULL'.  Numeric values are coerced to integer as by
          `as.integer' (and hence truncated towards zero).
    }
}

\value{
    A \code{db.Rquery} object is returned which is a SQL query to extract the
    requested subset.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
    \code{\link{[-methods}} Operator to extract elements
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
lk(x, 10)

lk(x[1:3])
lk(subset(x, 1:3))

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{data operation}
\keyword{database}
