\name{is.na-method}

\alias{is.na,db.obj-method}
\title{Query if the entries in a table are NULL}
\description{
 This function is equivalent to an SQL query that checks if the entries in a table are NULL.   
}
\usage{
\S4method{is.na}{db.obj}(x)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 The signature of the method.  A \code{db.obj} object.   
}
}
\details{
\code{is.na()} creates a \code{db.Rquery} object where the NULL entries in a \code{db.obj} object are \code{TRUE}, and other the entries are \code{FALSE}.  

}
\value{
The return value is a \code{db.Rquery} object.  
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
} 

\seealso{
  \code{\link{preview}} Displays the contents of a \code{db.obj} object.
}
\examples{
\dontrun{

## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- db.data.frame('abalone')

#Query which entries of x are NULL
is.na(x)

y <- x
y[is.na(y)] <- 3

z <- x
z[is.na(x$height),"height"] <- 23
}
}

\keyword{database}
\keyword{math}
\keyword{utility}
