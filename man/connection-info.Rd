\name{Extract database connection info}

\alias{connection info}
\alias{user}
\alias{host}
\alias{dbname}
\alias{dbms}
\alias{conn}
\alias{port}
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

port(conn.id = 1)

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

  For \code{port}, an integer, which is the port number of the connection.

  For \code{madlib}, a string, which is the MADlib version information.

  For \code{madlib.version}, a string, exactly the same as \code{madlib}.

  For \code{schema.madlib}, a string, which is the schema name of MADlib
  installation.

  For \code{conn.pkg}, a string, which is the name of the R package that
  has been used to connect to this database.
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

user(cid1)
host(cid2)
dbname(cid1) # use default connection 1
dbms(cid1)
madlib(cid1)
madlib.version(cid1)
schema.madlib(cid1)
conn.pkg(cid1)

## conn is mostly for other packages
con <- conn(cid1) # get the connection object
dbListTables(con) # directly use functions in package RPostgreSQL

## This package provides a better function to list all tables/views
db.objects(cid1) # list all tables/views with their schema in connection 1

db.disconnect(cid1, verbose = FALSE)
db.disconnect(cid2, verbose = FALSE)
}
}

\keyword{utility}
\keyword{database}
