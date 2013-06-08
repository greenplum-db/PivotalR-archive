\name{delete}

\alias{delete}
\alias{delete,db.data.frame-method}
\alias{delete,db.Rquery-method}
\alias{delete,character-method}

\title{ Delete a \code{db.obj} object or a table/view in the database }

\description{
  This function deletes a \code{db.data.frame} object together with the
  table/view that it points to. It deletes a \code{db.Rquery} object. It
  can also directly delete a table or view in the database.
}

\usage{
\S4method{delete}{db.data.frame}(x, cascade = FALSE)

\S4method{delete}{db.Rquery}(x)

\S4method{delete}{character}(x, conn.id = 1, is.temp = FALSE, cascade = FALSE)
}

\arguments{
  \item{x}{
    The signature of the method.

    A \code{db.data.frame} object, which
    points to a table or view in the database;

    or a \code{db.Rquery}
  object, which represents some operations on an existing
  \code{db.data.frame} object;

  or a string, the table/view name to delete in the database.  
}

\item{conn.id}{
  An integer, default is 1. The connection ID to the database.
}

\item{is.temp}{
  A logical, default is \code{FALSE}. Whether the table/view is temporary.
}

\item{cascade}{
  A logical, default is \code{FALSE}. Whether to delete objects together
  with all the objects depending on it.
}
}

\details{
  When a \code{db.data.frame} object is deleted, the table/view that is
  associated with it is also deleted.
}

\value{
  When \code{x} is \code{db.data.frame} or table/view name, this
  function returns a logical value. which is \code{TRUE} if the deletion
  is successful.

  No value is returned if \code{x} is \code{db.Rquery}
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{db.data.frame}} creates an object pointing to a table/view
  in the database.
  
  \code{\link{db.objects}} lists all tables and views in a database
  together with their schema.

  \code{\link{db.existsObject}} tests whether a table/view exists in the
  database.

  \code{\link{as.db.data.frame}} creates a \code{db.data.frame} from a
  \code{data.frame}, a data file or a \code{db.Rquery}.
}

\examples{
\dontrun{
## Suppose a valid connection 2 exists
delete("abalone", 2, is.temp = TRUE)

delete("abalone", 2, is.temp = FALSE)

x <- as.db.data.frame(abalone, "abalone", conn.id = 2)

preview(x, 10)

y <- as.db.data.frame(abalone, "abalone", conn.id = 2, is.temp = TRUE)

preview(y, 10)

db.existsObject("abalone", 2, is.temp = TRUE)

db.existsObject("abalone", 2, is.temp = FALSE)

delete("abalone", 2)

p <- db.objects()
p[p[,2] == "abalone",]
}
}

\keyword{methods}
\keyword{database}
\keyword{utilities}
