\name{db.data.frame}

\alias{db.data.frame}

\title{
  Create a \code{db.data.frame} object pointing to a table/view in the
  database
}

\description{
  This function creates an object of
  \code{\linkS4class{db.data.frame}}, which points to an existing
  table/view in the database. The operations that can be applied
  onto this class of objects are very similar to those of
  \code{data.frame}. No real data is loaded into R. The data transfered
  between the database and R is minimized, which is necessary when we
  deal with large data sets.
}

\usage{
db.data.frame(x, conn.id = 1, key = character(0), verbose = TRUE,
is.temp = FALSE)
}

\arguments{
  \item{x}{
    A string. It is the name of an existing table/view in the database.
  }

  \item{conn.id}{
    An integer, default is 1. The ID number of the database connection
    where the table resides.
  }

  \item{key}{
    A string, default is \code{character(0)}. The name of the primary
    key column. A primary key is a column in a table which must contain a
    unique value which can be used to identify each and every row of a
    table uniquely.
  }

  \item{verbose}{
    A logical, default is \code{TRUE}. Whether to print a short message
    when the object in the database is created.
  }

  \item{is.temp}{
    A logical, default is \code{FALSE}. Whether the existing table/view
    in the database is temporary.
  }
}

\value{
  A \code{db.data.frame} object. More precisely, a \code{db.table}
  object if it points to an existing table in the database, and a
  \code{db.view} object
  if it points to an existing view in the database.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{db.objects}} lists all tables and views in a database
  together with their schema.

  \code{\link{db.existsObject}} tests whether a table/view exists in the
  database.

  \code{\link{as.db.data.frame}} creates a \code{db.data.frame} from a
  \code{data.frame}, a data file or a \code{db.Rquery}.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname)

## create a table using as.db.data.frame
delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid)

## create an object pointing to the table
y <- db.data.frame("abalone", conn.id = cid)

## x and y point to the same table
eql(x, y) # returns TRUE

## create an object pointing to a table in a schema
db.q("create schema myschema", conn.id = cid)
z <- as.db.data.frame(abalone, "myschema.abalone", conn.id = cid)
db.q("drop schema myschema cascade", conn.id = cid)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{utility}
\keyword{database}
