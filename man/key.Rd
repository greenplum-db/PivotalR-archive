\name{key}
\alias{key}
\alias{key<-}

\title{
Get or set the primary key for a table
}
\description{
This function gets or sets the primary key for a \code{db.obj} table.
}
\usage{
key(x)

key(x) <- value

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 is a db.obj object.
}
\item{value}{
     must be a string.
}
}
\details{
\code{key()} will return the primary key of a table.  If the primary key is not set, \code{key()} will return the character \code{0}.  If \code{key()} is being used to set the primary key, then \code{value} must be a string, and it must match one of the column names in the table.

If this function is used to change the primary key to a new column name, this function does NOT check if all the values in that column are unique.
}
\value{
The return value is the primary key of the table.
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

## create a table from the example data.frame "abalone"
tmp <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
x <- db.data.frame(content(tmp), key = 'id', conn.id = cid, verbose = FALSE)

## getting the primary key
key(x) # Display the primary key for x

## Changing the primary key
key(x) <- 'length'

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{database}
\keyword{data operation}
