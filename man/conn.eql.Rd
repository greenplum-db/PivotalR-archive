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
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}, Hai Qian \email{hai.qian@gopivotal.com}
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
## Create two connections
db.connect(host = "localhost", user = "you", dbname = "your_db", port = 5432)
db.connect(user = "you", dbname = "your_db") # use default values

db.list() # list the above two connections

conn.eql(1, 2) # returns TRUE
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ database }

