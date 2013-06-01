\name{eql-methods}
\docType{methods}
\alias{eql}
\alias{eql,db.obj,db.obj-method}
\title{Test if two objects point to the same table}
\description{
This function checks if two \code{db.obj} objects are the equivalent.  For objects of type \code{db.data.frame}, they need to have the same associated table.  For objects of other types, they need to have identical expressions and the same associated table.
}
\usage{
\S4method{eql}{db.obj,db.obj}(e1,e2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e1}{
 The signature of the method. A db.obj object.   
}
\item{e2}{
 The signature of the method. A db.obj object.   
}
}
\details{
Objects of type \code{db.data.frame} are considered equal if they have the same \code{content} representation, and their associated tables have the same name, connection ID, and type.  Objects of other types derived from \code{db.obj} are considered equal if they have the same values for \code{content} representation, \code{@.source}, \code{@.parent}, \code{@.expression}, \code{@.where}, \code{@.conn.id}, \code{@.col.data_type}, \code{@.is.factor} and \code{@.col.name}.  Two objects of different types are always considered not equal.  
}
\value{
A logical.  Returns \code{true} is the objects are equal.  
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Predictive Analytics Team at Pivotal
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

y <- db.data.frame('abalone')

## Check for equality
eql(x,y) # This returns true

z <- x[,]

eql(x,y) # This returns false
}
}

\keyword{data}
\keyword{utilities}

