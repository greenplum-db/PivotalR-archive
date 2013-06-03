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
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
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
## connect to multiple databases
db.connect(dbname = "mydb", port = 5433, madlib = "madlib")
db.connect(dbname = "otherdb", port = 5432, madlib = "madlib07")

db.list() # list the two connections
}
}

\keyword{utilities}
\keyword{ database }
\keyword{ misc }