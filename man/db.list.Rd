\name{db.list}

\alias{db.list}

\title{
  List all the currently active connections with their information
}
\description{
  List all the currently active connections with their information
  including the connection ID, host, user, database, DBMS (database
  management system), MADlib schema name in the database, and the R
  package name used to connect to the database.
}
\usage{
db.list()
}

\value{
  No value is returned.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\note{
  Currently, only connection to PostgreSQL and Greenplum databases are
  supported. Support for other types of DBMS's will be added in the future.
}

\seealso{
  \code{\link{db.connect}} connects to database.

  \code{\link{db.disconnect}} disconnects a connection.

  \code{\link{connection info}} the functions that extract information
  about the connection.

  \code{\link{conn.eql}} tests whether two connections are the same.
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

db.disconnect(cid1, verbose = FALSE)
db.disconnect(cid2, verbose = FALSE)
}
}

\keyword{utility}
\keyword{database}
