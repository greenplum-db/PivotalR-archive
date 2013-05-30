\name{db.connect}
\alias{db.connect}

\title{
  Create a connection to a database
}
\description{
  Create a connection to a database.
}
\usage{
db.connect(host = "localhost", user = Sys.getenv("USER"), dbname = user,
password = "", port = 5432, madlib = "madlib", conn.pkg = "RPostgreSQL")
}

\arguments{
  \item{host}{
    A string, default is "localhost". The name or IP of the host where
  the database is located.
}
  \item{user}{
    A string, default is the user's username. The username used to
  connect to the database.
}
  \item{dbname}{
    A string,  default is the same as the username. The name of the
  database that you want to connect to.
}
  \item{password}{
    A string, default is "". The password string used to connect to the database.
   }
   \item{port}{
     An integer, default is 5432. The port number used to connect to the database.
   }
   \item{madlib}{
     A string, default is "madlib". The name of the schema where MADlib
  is installed.
   }
  \item{conn.pkg}{
    A string, default is "RPostgreSQL". The name of the R package used
  to connect to the database. Currently, only RPostgreSQL is supported,
  but the support for other packages such as RODBC can be easily added.
   }
}
\details{
  
}

\value{
  An integer, the ID number for the newly created connection.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  
}
\examples{
\dontrun{

}
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
