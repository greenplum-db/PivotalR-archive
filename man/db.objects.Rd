\name{db.objects}

\alias{db.objects}

\title{
  List all the existing tables/views in a database with their schema names
}

\description{
  This function lists all the existing tables and views in a database,
  together with their schema names
}

\usage{
db.objects(search = NULL, conn.id = 1)
}

\arguments{
  \item{search}{
    A string, default is \code{NULL}. List all database objects whose
  names have the string in them. You can put regular expression here.
  }
  
  \item{conn.id}{
    An integer, default is 1. The ID of the database connection.
  }
}

\value{
  A character array. Each element has the format of
  'schema_name.table_name'. 
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}, Hai Qian \email{hai.qian@gopivotal.com}
}

\seealso{
  \code{\link{db.connect}} creates a connection to a database.

  \code{\link{db.existsObject}} tests whether an object exists in the database
}
\examples{
\dontrun{
## create connection to a database
conid <- db.connect(port = 5433) # using default values

## create a table using as.db.data.frame
x <- as.db.data.frame(abalone, "abalone", conn.id = conid)

db.objects(conid) # list all tables/views

## list all tables/views start with "madlibtestdata.lin"
## where "madlibtestdata" is the schema name
db.objects("^madlibtestdata.lin") 
}
}

\keyword{ utilities }
\keyword{ misc }
