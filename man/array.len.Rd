\name{array.len}
\alias{array.len}

\title{Get the length of the array in an array column}
\description{The column of a table in database can be an array. This   function measures the length of the array.}

\usage{
array.len(x)
}

\arguments{
  \item{x}{
    A \code{\linkS4class{db.obj}} object.
  }
}

\value{
  An integer, which is the length of the array.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{db.array}} combines columns of a table/view into an
  array.

  \code{\link{as.list}} expands the \code{db.obj} columns into a list of   separated \code{db.Rquery} objects.

\code{\link{cbind2}} and \code{\link{cbind}} combine multiple \code{db.obj} objects into one \code{db.obj} object.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name

## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

delete("abalone", conn.id = cid)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid, verbose = FALSE)
y <- db.array(x[-2]) # put columns into an array
names(y) # "agg_opr"
array.len(y$agg_opr) # 9

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{data operation}
\keyword{utility}