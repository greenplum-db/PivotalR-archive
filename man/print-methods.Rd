\name{print}
\alias{print,db.data.frame-method}
\alias{print,db.Rquery-method}

\title{Display the connection information associated with a \code{db} object}

\description{
  This function displays the SQL table, database, host, and connection
information associated with a \code{db.table} or \code{db.view} object.
}

\usage{
  \S4method{print}{db.data.frame}(x)
  \S4method{print}{db.Rquery}(x)
}

\arguments{
  \item{x}{
    The signature of the method.
    A \code{db.data.frame} (includes \code{db.table} and \code{db.view})
    object, which points to a table or view in the database; or a \code{db.Rquery}
    object, which represents some   operations on a \code{db.data.frame} object.
  }
}

\details{
  When the signature \code{x} is either a \code{db.data.frame} object or a
  \code{db.Rquery} object, this function displays the name of connected SQL
  database, the SQL database host, and the connection ID.


	When the signature \code{x} is a \code{db.data.frame} object, the function also displays the associated table.  When the signature \code{x} is a \code{db.Rquery} object, this
  function displays the temporary status of the input, and the table that it is derived from.  }

\value{
  This function returns nothing. 
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{preview}} Displays the contents of an associated table.
}
\examples{
\dontrun{

## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- as.db.data.frame(abalone, "abalone")

## printing db.data.frame object
print(x) # Display the associated table, and database information for x

##Alternative way of doing the same thing
x# Display the associated table, and database information for x

}
}
\keyword{IO}
\keyword{print}



