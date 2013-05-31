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

  
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}, Hai Qian \email{hai.qian@gopivotal.com}
}

\seealso{
objects to See Also as \code{\link{help}}, ~~~
}

\examples{
\dontrun{

}
}

\keyword{ IO & database }
\keyword{ utilities }
