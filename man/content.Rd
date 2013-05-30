\name{content}
\alias{content}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Print the content of a \code{db.obj} object
}
\description{
  A \code{db.data.frame} object's content is the table/view name that it
  points to. A \code{db.Rquery} object's content is the SQL query that
  represents the operations applied on an existing
  \code{db.data.frame}. This function is mainly for debugging. Normal
  user who is not familiar with SQL does not need to use it.
}
\usage{
content(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A \code{db.obj} object, whose content will be returned.
}
}

\value{
  A string, the content of \code{db.obj} object \code{x}.   A
  \code{db.data.frame} object's content is the table/view name that it 
  points to. A \code{db.Rquery} object's content is the SQL query that
  represents the operations applied on an existing \code{db.data.frame}.
}

\author{
   Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}, Hai Qian \email{hai.qian@gopivotal.com}
}


\seealso{
  \code{\link{db.obj-class}}, \code{\link{db.data.frame-class}},
  \code{\link{db.table-class}}, \code{\link{db.view-class}},
  \code{\link{db.Rquery-class}} explain the definitions of the class
  hierarchy of this package.
}

\examples{
\dontrun{

## create a database connection using default settings
db.connect()

y <- as.db.data.frame(abalone, "abalone") # create a table
x <- db.data.frame("abalone")

## actually, x and y are pointing the same table
eql(x, y) # returns TRUE

content(x)
content(x$id)
content(x$id < 10)
content(x[,1:5])
content(x == y) # this is different from eql(x, y)
content(sort(x, by = "id"))
content(x[x$id<10,])
content(x[1:10,])
content(colSums(x))
content(by(x, NULL, sum))
content(by(x, x$sex, sum))
}
}

\keyword{ sql }
\keyword{ database }
