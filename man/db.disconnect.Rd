\name{db.disconnect}

\alias{db.disconnect}

\title{
  Disconnect a connection to a database
}
\description{
   Although all the database connections will be automatically closed
  when this package is unloaded, one can choose to disconnect a database
  connection himself.
}

\usage{
db.disconnect(conn.id = 1, verbose = TRUE, force = FALSE)
}

\arguments{
  \item{conn.id}{
    An integer, the ID of the connection that you want to disconnect.
   }

   \item{verbose}{
     A logical, default is \code{TRUE}. Whether to print a message
     during disconnection.
   }

   \item{force}{
     A logical, default is \code{FALSE}. Whether to remove the
     connection forcefully. This is useful when you lose the connection
     and cannot disconnect the connection normally.
   }
}

\value{
  A logical, \code{TRUE} if the connection is successfully disconnected.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{db.connect}} creates the database connection.

  \code{\link{db.list}} lists all active connections.

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
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

db.list()

## disconnect the connection
db.disconnect(cid, verbose = FALSE)

db.list()
}
}

\keyword{utility}
\keyword{database}
\keyword{connection}