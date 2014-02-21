\name{is.na-method}

\alias{is.na,db.obj-method}
\title{Query if the entries in a table are NULL}
\description{
 This function is equivalent to an SQL query that checks if the entries in a table are NULL.
}
\usage{
\S4method{is.na}{db.obj}(x)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 The signature of the method.  A \code{db.obj} object.
}
}
\details{
\code{is.na()} creates a \code{db.Rquery} object where the NULL entries in a \code{db.obj} object are \code{TRUE}, and other the entries are \code{FALSE}.

}
\value{
The return value is a \code{db.Rquery} object.
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{lk}} or \code{\link{lookat}} Displays the contents of a \code{db.obj} object.
}
\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a temp table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

#Query which entries of x are NULL
is.na(x)

y <- x
y[is.na(y)] <- 3

z <- x
z[is.na(x$height),"height"] <- 23

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{database}
\keyword{math}
\keyword{utility}
