\name{conn.id}
\alias{conn.id}
\alias{conn.id<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Find out the connection ID of a \code{db.obj} object
}
\description{
  Each \code{db.obj} object contains the ID of the connection that its
  data resides on. This function returns the connection ID number. The
  user can also change the connection ID that a \code{db.obj} is
  associated with.
}
\usage{
conn.id(x)
conn.id(x) <- value
}

\arguments{
  \item{x}{
    A \code{db.obj} object.
}
\item{value}{
    An integer, the connection ID number. The user is allowed to change
  the connection ID that is associated with \code{x}.
}
}

\value{
  An integer, the connection ID associated with \code{x}
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{db.connect}} creates connections to the databases.

  \code{\link{db.disconnect}} disconnects an existing connection.

  \code{\link{db.list}} lists all the current connections with their
  information.

 \code{\link{connection info}} has all functions that can extract
  information about the database connection.

  \code{\link{conn.eql}} tests whether two connections are actually the
  same one.
}
\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid1 <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)
cid2 <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

db.list() # list the two connections

conn.eql(cid1, cid2) # returns TRUE

## use the example data to create a table in connection 1
delete("abalone", conn.id = cid2)
x <- as.db.data.frame(abalone, "abalone", conn.id = cid1, verbose = FALSE)

db.disconnect(cid1) # disconnect connection 1

## lookat(x) # gives an error since connection 1 is disconnected

conn.id(x) <- cid2 # 1 and 2 are the same

lk(x) # gives what you want

db.disconnect(cid2, verbose = FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{database}
\keyword{utility}
