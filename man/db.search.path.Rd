\name{db.search.path}
\alias{db.search.path}
\alias{db.default.schemas}

\title{
  Display or set the search path (i.e. default schemas) for a connected
  session to a database
}

\description{
 
}

\usage{
db.search.path(conn.id = 1, set = NULL)

db.default.schemas(conn.id = 1, set = NULL)
}

\arguments{
  \item{conn.id}{

  }

  \item{set}{

  }
}

\value{
  
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{db.connect}} connects to database, and the parameter
  \code{default.schemas} can be used to set the search path when
  connecting. 
}

\examples{
\dontrun{

}
}

\keyword{utilities}
\keyword{ database }
\keyword{ misc }