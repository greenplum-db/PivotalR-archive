\name{Type Cast functions}

\alias{Type Cast functions}
\alias{as.integer}
\alias{as.character}
\alias{as.double}
\alias{as.logical}
\alias{as.numeric}
\alias{as.integer,db.obj-method}
\alias{as.character,db.obj-method}
\alias{as.double,db.obj-method}
\alias{as.logical,db.obj-method}
\alias{as.numeric,db.obj-method}
\alias{as.Date,db.obj-method}
\alias{db.date.style}
\alias{as.time}
\alias{as.timestamp}
\alias{as.interval}
\alias{col.types}

\title{Cast columns of \code{db.obj} objects to other types}

\description{
  Coerce \code{\linkS4class{db.obj}} object columns into other types. \code{col.types} displays the types of each column. \code{as.Date} converts to date (no time of day); \code{as.time} converts to time of day (no date); \code{as.timestamp} converts to both date and time; \code{as.interval} converts to time interval. \code{db.date.style} can display or set the date style for a particular connection.
}

\usage{
\S4method{as.integer}{db.obj}(x, ...)

\S4method{as.character}{db.obj}(x, array = TRUE, ...)

\S4method{as.double}{db.obj}(x, ...)

\S4method{as.logical}{db.obj}(x, ...)

\S4method{as.numeric}{db.obj}(x, ...)

\S4method{as.Date}{db.obj}(x, ...)

db.date.style(conn.id = 1, set = NULL)

as.time(x, ...)

as.timestamp(x, ...)

as.interval(x, ...)

col.types(x)
}

\arguments{
    \item{x}{
      A \code{\linkS4class{db.obj}} object. All columns of the object will be converted into the target type. If the column contains arrays, the array will be converted into an array of the target type.
    }

    \item{array}{
      A logical, default is \code{TRUE}. If \code{array} is \code{TRUE}, then an array column is converted into an array of strings. If \code{array} is \code{FALSE}, then an array column is converted into a string column, i.e. \code{array[1,2,3]} is casted into \code{'\{1,2,3\}'}.
    }

    \item{\dots}{
      further arguments passed to or from other methods
      This is currently not implemented.
    }

    \item{conn.id}{
      An integer, default is 1. The connection ID for the database connection.
    }

    \item{set}{
      A string, default is \code{NULL}. It can be \code{"us"} or \code{"european"}. If it is \code{NULL}, \code{db.date.style} displays the current date style in the connected database. Otherwise, it sets the date style for the connected database.
    }
}

\value{
    A \code{\linkS4class{db.Rquery}} object, which is a SQL query which     combine all columns into an array.

\code{col.types} returns a vector of characters, which are the column types of \code{x}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
    \code{\link{by,db.obj-method}} is usually used together with
    aggregate functions.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
lk(x, 10)

z <- as.integer(x > 1)
lookat(z, 10)

z <- as.integer(x[,2] == "M")
lookat(z, 10)

col.types(x)

col.types(z)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{utility}
\keyword{data operation}