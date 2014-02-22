\name{ifelse}

\alias{ifelse,db.obj-method}

\title{Conditional Element Selection}

\description{
     'ifelse' returns a value with the same shape as 'test' which is
     filled with elements selected from either 'yes' or 'no' depending
     on whether the element of 'test' is 'TRUE' or 'FALSE'.
}

\usage{
\S4method{ifelse}{db.obj}(test, yes, no)
}

\arguments{
    \item{test}{
      A \code{db.obj} object, which has only one column. The column can be casted into boolean values.
    }

    \item{yes}{
      A normal value or a \code{db.obj} object. It is the returned value when \code{test} is \code{TRUE}.
    }

    \item{no}{
      The returned value when \code{test} is \code{FALSE}.
    }
}

\value{
  A \code{\linkS4class{db.obj}} which has the same length in-database as   \code{test}.
}

\author{
  Author: Hong Ooi, Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\linkS4class{db.obj}}
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table
x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

## create a new db.obj with one-column,
## and values "small" or "big"
z <- ifelse(x$rings < 10, "small", "big")

db.disconnect(cid, verbose = FALSE)
}
}