\name{Extract database connection info}

\alias{user}
\alias{host}
\alias{dbname}
\alias{dbms}
\alias{conn}
\alias{madlib}
\alias{madlib.version}
\alias{schema.madlib}
\alias{conn.pkg}

\title{
  Utilities for extracting related information about a database connection
}

\description{
  For a given database connection, these functions return the user name,
  host, database name, info about database management system,
  connection, the version of MADlib installed on this database, the
  schema name of MADlib installation, and the R package that is used to
  connect to this database.
}

\usage{
user(conn.id = 1)

host(conn.id = 1)

dbname(conn.id = 1)

dbms(conn.id = 1)

conn(conn.id = 1)

madlib(conn.id = 1)

madlib.version(conn.id = 1)

schema.madlib(conn.id = 1)

conn.pkg(conn.id = 1)
}

\arguments{
  \item{conn.id}{
    Default value is 1. The database connection ID number
    \code{con.id}. It is an integer.
  }
}

\value{
  For \code{user}, a string, which is the user name.
  
  For \code{host}, a string, which is the host address.

  For \code{dbname}, a string, which is the database name.

  For \code{dbms}, a string, which is DBMS version information.

  For \code{conn}, an object of DBI connection, which can be directly
  used with packages such as RPostgreSQL.

  For \code{madlib}, a string, which is the MADlib version information.

  For \code{madlib.version}, a string, exactly the same as \code{madlib}.

  For \code{schema.madlib}, a string, which is the schema name of MADlib
  installation.

  For \code{conn.pkg}, a string, which is the name of the R package that
  has been used to connect to this database.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}, Hai Qian \email{hai.qian@gopivotal.com}
}

\examples{
\dontrun{

user(1)
host(2)
dbname() # use default connection 1
dbms()
madlib()
madlib.version()
schema.madlib()
conn.pkg()

con <- conn(1) # get the connection object
dbListTables(con) # directly use functions in RPostgreSQL
}
}

\keyword{ utility }
\keyword{ info }
\keyword{ database }

