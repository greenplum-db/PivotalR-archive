\name{preview}

\alias{preview}

\title{
  Read the actual data stored in the table of database
}
\description{
  The function reads the actual data from the table in the database.
}
\usage{
preview(x, nrows = 100, interactive = TRUE)
}

\arguments{
  \item{x}{
    A \code{db.data.frame} object, which points to a table or view in
    the database.
  }
  \item{nrows}{
    An integer, default is 100, how many rows of data 
  }
  \item{interactive}{
    A boolean, default is TRUE. If \code{x} is a view, then extracting
  data from it would actually compute the view, which might take a
  longer time, especially for large data sets. When \code{interactive}
  is TRUE, this function will ask the user whether to continue to
  extract data from the view. 
  }
  
}
\value{
  A \code{data.frame} object. Each column in the table becomes a column
  of the returned \code{data.frame}. A column of arrays is converted into
  a column of strings, see \code{\link{arraydb.to.arrayr}} for more
  details. 
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}, Hai Qian \email{hai.qian@gopivotal.com}
}

\seealso{
  \code{\link{arraydb.to.arrayr}} convert strings extracted form
  database into arrays
}
\examples{
\dontrun{

x <- db.data.frame("table_in_database", conn.id = 1)
preview(x, nrows = 10, interactive = FALSE) # extract 10 rows of data
}
}

\keyword{ database }
\keyword{ utility }
