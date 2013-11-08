\name{is.db.data.frame}
\alias{is.db.data.frame}

\title{
Check if an object is of type \code{db.data.frame}
}
\description{
This function checks if the input is of type \code{db.data.frame}.
}
\usage{
is.db.data.frame(x)
}

\arguments{
  \item{x}{
The input can be of any type.  
}
}

\details{
\code{is.db.data.frame()} returns \code{TRUE} if \code{x} is of type \code{db.data.frame}.  Otherwise, it returns \code{FALSE}.  

}
\value{
A logical.  Returns   \code{TRUE} if the input is of type \code{db.data.frame}
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc.
  
  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
} 

\seealso{
  \code{\link{as.db.data.frame}} Convert an object into another object of type \code{db.data.frame}.
}
\examples{
\dontrun{

## Suppose that a valid connection with ID 1 exists

## create a table from the example data.frame "abalone"
x <- db.data.frame('abalone', key = 'id')

## getting the primary key
is.db.data.frame(x) #check if x is of type db.data.frame

}
}

\keyword{database}
\keyword{utility}
