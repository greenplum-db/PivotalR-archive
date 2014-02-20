\name{conn.eql}
\alias{conn.eql}

\title{
  Check whether two connections are the same
}

\description{
  Two connections are regarded as equal if and only if they have the
  same database name, host, DBMS, and port number.
}

\usage{
conn.eql(conn.id1, conn.id2)
}

\arguments{
  \item{conn.id1}{
    An integer, a connection ID number.
}
  \item{conn.id2}{
    An integer, another connection ID number.
}
}

\value{
  A logical. TRUE if and only if the two connections have the
  same database name, host, DBMS, and port number.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{connection info}} has all functions that can extract
  information about the database connection.

  \code{\link{db.connect}} creates connections to the databases.

  \code{\link{db.disconnect}} disconnects an existing connection.

  \code{\link{db.list}} lists all the current connections with their
  information.
}
\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid1 <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)
cid2 <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

db.list() # list the above two connections

conn.eql(cid1, cid2) # returns TRUE

db.disconnect(cid1, verbose = FALSE)
db.disconnect(cid2, verbose = FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{database}
\keyword{utility}
