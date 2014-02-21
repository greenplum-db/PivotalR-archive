\name{names-methods}

\alias{names,db.obj-method}
\alias{names<-,db.obj-method}

\title{The Names of an object}
\description{
    This function gives the names of a \code{db.obj}, which are the column
    names of a \code{db.table} or \code{db.view}. The names are returned as a list
}

\usage{
\S4method{names}{db.obj}(x)
\S4method{names}{db.obj}(x) <- value
}


\arguments{
    \item{x}{
        A \code{db.obj}.
        The input data frame for which the column names are required.
      }

      \item{value}{
        An array of strings. New names to replace the names of \code{x}.
      }
}

\value{
    Returns a string with the list of the column names of data
    frame. The names are ordered.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\linkS4class{db.obj}}, \code{\linkS4class{db.data.frame}},
  \code{\linkS4class{db.table}}, \code{\linkS4class{db.view}},
  \code{\linkS4class{db.Rquery}} are the class hierarchy structure of
  this package.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table from the example data.frame "abalone"
delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid, verbose = FALSE)
## preview of a table
lk(x, nrows = 10) # extract 10 rows of data

## get names of all columns
names(x)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{database}