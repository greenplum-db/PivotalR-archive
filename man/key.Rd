\name{key}
\alias{key}
\alias{key<-}

\title{
Get or set the primary key for a table
}
\description{
This function gets or sets the primary key for a \code{db.obj} table.  
}
\usage{
key(x)

key(x) <- value

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 is a db.obj object.   
}
\item{value}{
     must be a string.  
}
}
\details{
\code{key()} will return the primary key of a table.  If the primary key is not set, \code{key()} will return the character \code{0}.  If \code{key()} is being used to set the primary key, then \code{value} must be a string, and it must match one of the column names in the table.  

If this function is used to change the primary key to a new column name, this function does NOT check if all the values in that column are unique. 
}
\value{
The return value is the primary key of the table.  
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
x <- db.data.frame('abalone', key = 'id')

## getting the primary key
key(x) # Display the primary key for x

## Changing the primary key
key(x) <- 'length'

}
}

\keyword{database}
\keyword{data operation}


