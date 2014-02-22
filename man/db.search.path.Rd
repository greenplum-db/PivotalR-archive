\name{db.search.path}
\alias{db.search.path}
\alias{db.default.schemas}

\title{
  Display or set the search path (i.e. default schemas) for a connected
  session to a database. The use can easily switch to a schema that he
  has the privilege to write.
}

\description{
  Allow the user to check and set the search path for the session that
  he connects to the database. The search path is a set of schema names
  separated by commas. These are the default schemas that the programme
  will search and save tables if a schema name is not given together
  with the table name in the format of "schema_name.table_name".
}

\usage{
db.search.path(conn.id = 1, set = NULL)

db.default.schemas(conn.id = 1, set = NULL)
}

\arguments{
  \item{conn.id}{
    An integer, default is 1. The ID of the database connection.
  }

  \item{set}{
    A string, default is \code{NULL}. The default schema names separated
    by commas.
  }
}

\value{
  When \code{set} is \code{NULL}, this function prints the current
  connected session's search path.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{db.connect}} connects to database, and the parameter
  \code{default.schemas} can be used to set the search path when
  connecting.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE, default.schemas =
"public,madlib")

db.search.path()

db.search.path(set = "public,madlibtestdata")

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{utility}
\keyword{database}
