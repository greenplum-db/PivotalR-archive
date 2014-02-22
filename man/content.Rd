\name{content}
\alias{content}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Print the content of a \code{db.obj} object
}
\description{
  A \code{db.data.frame} object's content is the table/view name that it
  points to. A \code{db.Rquery} object's content is the SQL query that
  represents the operations applied on an existing
  \code{db.data.frame}. This function is mainly for debugging. Normal
  user who is not familiar with SQL does not need to use it.
}
\usage{
content(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A \code{db.obj} object, whose content will be returned.
}
}

\value{
  A string, the content of \code{db.obj} object \code{x}.   A
  \code{db.data.frame} object's content is the table/view name that it
  points to. A \code{db.Rquery} object's content is the SQL query that
  represents the operations applied on an existing \code{db.data.frame}.
}

\author{
   Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}


\seealso{
  \code{\linkS4class{db.obj}}, \code{\linkS4class{db.data.frame}},
  \code{\linkS4class{db.table}}, \code{\linkS4class{db.view}},
  \code{\linkS4class{db.Rquery}} explain the definitions of the class
  hierarchy of this package.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

delete("abalone", conn.id = cid)
y <- as.db.data.frame(abalone, "abalone", conn.id = cid) # create a table
x <- db.data.frame("abalone", conn.id = cid, key = "id")

## actually, x and y are pointing the same table
eql(x, y) # returns TRUE

content(x)
content(x$id)
content(x$id < 10)
content(x[,1:5])
content(x == y) # this is different from eql(x, y)
content(sort(x, INDICES = x$id))
content(x[x$id<10,])
content(x[1:10,])
content(colSums(x))
content(by(x, NULL, sum))
content(by(x, x$sex, sum))

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{utility}
\keyword{database}
