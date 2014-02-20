\name{db.existsObject}

\alias{db.existsObject}

\title{
  Test whether an object exists in the database
}
\description{
  Test whether a table or view exists in the database
}
\usage{
db.existsObject(name, conn.id = 1, is.temp = FALSE)
}

\arguments{
  \item{name}{
    A string, the name of table or view
  }

  \item{conn.id}{
    An integer, default is 1. The ID of the database connection.
  }

  \item{is.temp}{
    A logical, default is \code{FALSE}. Whether this table/view is
    a temporary object.
  }
}

\value{
  This function returns different types of results depending the input.

  If \code{name} has the format of \code{myschema.mytable}, the return
  value is a logical. It is TRUE if the table/view exists in the
  database.

  If \code{name} has the format of \code{mytable} and \code{is.temp =
    FALSE}, the return value is also a logical, which is TRUE if the table/view exists in the
  database.

  If \code{name} has the format of \code{mytable} and \code{is.temp =
    TRUE}, the return value is a list. The list has two elements. The
    first is a logical, which is TRUE if the table/view exists in the
  database. The second is a character array with 2 elements, whose first
    is the temporary schema name and the second is the table/view name.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
objects to See Also as \code{\link{help}}, ~~~
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

db.list()

db.existsObject("madlibtestdata.lin_ornstein", cid)

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{database}
\keyword{utility}
