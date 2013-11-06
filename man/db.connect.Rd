\name{db.connect}
\alias{db.connect}

\title{
  Create a connection to a database
}
\description{
  Create a connection to a PostgreSQL or Greenplum (Pivotal) database. One can create multiple connections
  to multiple databases. The connections are indexed by an integer
  starting from 1.
}
\usage{
db.connect(host = "localhost", user = Sys.getenv("USER"), dbname = user,
password = "", port = 5432, madlib = "madlib", conn.pkg = "RPostgreSQL",
default.schemas = NULL)
}

\arguments{
  \item{host}{
    A string, default is "localhost". The name or IP of the host where
  the database is located.
}
  \item{user}{
    A string, default is the user's username. The username used to
  connect to the database.
}
  \item{dbname}{
    A string,  default is the same as the username. The name of the
  database that you want to connect to.
}
  \item{password}{
    A string, default is "". The password string used to connect to the database.
   }
   \item{port}{
     An integer, default is 5432. The port number used to connect to the database.
   }
   \item{madlib}{
     A string, default is "madlib". The name of the schema where MADlib
  is installed.
   }
  \item{conn.pkg}{
    A string, default is "RPostgreSQL". The name of the R package used
    to connect to the database. Currently, only RPostgreSQL is supported,
    but the support for other packages such as RODBC can be easily added.
  }
  \item{default.schemas}{
    A string, default is \code{NULL}. The search path or default schemas
    of the database that you want to use. The string must be a set of
  schema names 
    separated by commas. One can also use
  \code{\link{db.default.schemas}} or
  \code{\link{db.search.path}}  to display or set the search 
  path in the database.
  }
 }

\value{
  An integer, the ID number for the newly created connection.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\note{
  Right only MADlib 0.6 or later is supported. If you have an older
  version of MADlib, you will not be able to use all the functions whose
  names start with "madlib.". However you can still use all the other
  functions.

  Also, right now only PostgreSQL and Greenplum databases are supported.
}

\seealso{
  \code{\link{db.disconnect}} disconnects a connection.

  \code{\link{db.list}} lists all active connections.

  \code{\link{connection info}} the functions that extract information
  about the connection.
  
  \code{\link{conn.eql}} tests whether two connections are the same.

  \code{\link{db.search.path}} and \code{\link{db.default.schemas}}
  displays or sets the search path (i.e. default schemas) in the
  connected database.
}
\examples{
\dontrun{
## connect to a database
db.connect(host = "localhost", user = "me", dbname = "me", password =
"", port = 5432, madlib = "madlib")
}
}

\keyword{utility}
\keyword{connection}
\keyword{database}
