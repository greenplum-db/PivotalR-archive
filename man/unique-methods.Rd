\name{unique-methods}

\alias{unique,db.obj-method}

\title{The Unique of an object}
\description{
    This function gives the unique values of a \code{db.obj}, which are
    the column
    unique of a \code{db.table} or \code{db.view}.
}

\usage{
\S4method{unique}{db.obj}(x, incomparables = FALSE, ...)
}

\arguments{
    \item{x}{
        A \code{db.obj} object, which the column unique are to be computed. The object has to have only one column otherwise an error will be raised.
      }

      \item{incomparables}{
        Not implemented.
      }

      \item{\dots}{
        Not implemented.
      }
}

\value{
    An \code{\linkS4class{db.Rquery}}, whose column is the unique value of the column.
}

\note{This function applies only onto \code{\linkS4class{db.obj}} with
  one column. If you want to put the unique values from multiple columns
  together, you have to use \code{\link{db.array}}}

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

x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
lk(x, 10)

## get unique of all columns
unique(x$sex)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{methods}
\keyword{database}