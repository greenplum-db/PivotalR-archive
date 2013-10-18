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
\alias{col.types}

\title{Cast columns of \code{db.obj} objects to other types}

\description{
  Coerce \code{\linkS4class{db.obj}} object columns into other types. \code{col.types} displays the types of each column.
}

\usage{
\S4method{as.integer}{db.obj}(x, ...)

\S4method{as.character}{db.obj}(x, ...)

\S4method{as.double}{db.obj}(x, ...)

\S4method{as.logical}{db.obj}(x, ...)

\S4method{as.numeric}{db.obj}(x, ...)

col.types(x)
}

\arguments{
    \item{x}{
      A \code{\linkS4class{db.obj}} object. All columns of the object will be converted into the target type. If the column contains arrays, the array will be converted into an array of the target type.
    }

    \item{\dots}{
      further arguments passed to or from other methods
      This is currently not implemented.
    }
}

\value{
    A \code{\linkS4class{db.Rquery}} object, which is a SQL query which     combine all columns into an array.

\code{col.types} returns a vector of characters, which are the column types of \code{x}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.   \email{user@madlib.net}

Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
}

\seealso{
    \code{\link{by,db.obj-method}} is usually used together with
    aggregate functions.
}

\examples{
\dontrun{
## Suppose that a valid connection to a database (with ID=1) is established
## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")

z <- as.integer(x > 1)
lookat(z, 10)

z <- as.integer(x[,2] == "M")
lookat(z, 10)

col.types(x)

col.types(z)
}
}

\keyword{utility}
\keyword{data operation}