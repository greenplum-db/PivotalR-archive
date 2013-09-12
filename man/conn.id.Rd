\name{conn.id}
\alias{conn.id}
\alias{conn.id<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Find out the connection ID of a \code{db.obj} object
}
\description{
  Each \code{db.obj} object contains the ID of the connection that its
  data resides on. This function returns the connection ID number. The
  user can also change the connection ID that a \code{db.obj} is
  associated with.
}
\usage{
conn.id(x)
conn.id(x) <- value
}

\arguments{
  \item{x}{
    A \code{db.obj} object.
}
\item{value}{
    An integer, the connection ID number. The user is allowed to change
  the connection ID that is associated with \code{x}.
}
}

\value{
  An integer, the connection ID associated with \code{x}
}

\author{
    Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}, Hai Qian \email{hai.qian@gopivotal.com}
}

\seealso{
  \code{\link{db.connect}} creates connections to the databases.

  \code{\link{db.disconnect}} disconnects an existing connection.

  \code{\link{db.list}} lists all the current connections with their
  information.

 \code{\link{connection info}} has all functions that can extract
  information about the database connection.
  
  \code{\link{conn.eql}} tests whether two connections are actually the
  same one.
}
\examples{
\dontrun{
## create two connections
db.connect(host = "localhost", user = "you", dbname = "your_db", port = 5432)
db.connect(user = "you", dbname = "your_db") # use default values

do.list() # list the two connections

conn.eql(1,2) # returns TRUE

## use the example data to create a table in connection 1
x <- as.db.data.frame(abalone, "abalone")

db.disconnect(1) # disconnect connection 1

preview(x) # gives an error since connection 1 is disconnected

conn.id(x) <- 2 # 1 and 2 are the same

preview(x) # gives what you want
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{database}
\keyword{utility}
