\name{db.q}
\alias{db}
\alias{.db}
\alias{db.q}

\title{
  Execute a SQL query
}
\description{
  This function sends SQL queries into the connected database to execute, and then extracts the result if there is any.
}
\usage{
db(..., nrows = 100, conn.id = 1, sep = " ", verbose = TRUE)

.db(..., nrows = 100, conn.id = 1, sep = " ", verbose = TRUE)

db.q(..., nrows = 100, conn.id = 1, sep = " ", verbose = TRUE)
}

\arguments{
  \item{\dots}{
    One or multiple SQL query strings. Multiple strings will be concatenated into one SQL query string.
  }

  \item{nrows}{
    An integer, default is 100. How many rows should be extracted? If it is \code{NULL}, \code{"all"} or non-positive value, all rows in the result will be loaded into R. For big dataset, you may not want to do this.
  }

  \item{conn.id}{
    An integer, default is 1. The ID of the connection. See \code{\link{db.list}} for how to list the existing database connections.
  }

  \item{sep}{
    A string, default is a space character \code{" "}. If multiple strings are used in \code{\dots}, this string is used to separate them in the concatenation.
  }

  \item{verbose}{
    A logical, default is \code{TRUE}. Whether to output the SQL query that you are executing.
  }
}

\value{
  A \code{data.frame} that contains the result if the result is not empty. Otherwise, it returns a logical value, which indicates whether the SQL query has been sent to the database successfully.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}


\seealso{
  \code{\link{db.connect}}, \code{\link{db.objects}},   \code{\link{db.list}},
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

db("show search_path", conn.id = cid)
.db("drop table if exists tr;",
    "create temp table tr (idx integer,
                           val double precision);",
    "insert into tr values (1, 2.3), (2, 3.4)", conn.id = cid)
db.q("select * from tr", conn.id = cid)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{utility}
\keyword{database}
