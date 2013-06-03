\name{db.obj-class}
\Rdversion{1.1}
\docType{class}
\alias{db.obj-class}

\title{Abstract Class \code{"db.obj"}}
\description{
  The super class of \code{\linkS4class{db.data.frame}} and
  \code{\linkS4class{db.Rquery}} 
}

\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}

\section{Methods}{
  See \code{\linkS4class{db.data.frame}} for all the available methods
  and functions.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
  \code{\link{db.data.frame}} creates a \code{db.data.frame} object.

  \code{\link{as.db.data.frame}} converts \code{db.Rquery} object,
  \code{data.frame}, or a data file into a \code{db.data.frame} object
  and at the same time creates a new table in the database.

  \code{\linkS4class{db.data.frame}} and \code{\linkS4class{db.Rquery}}
  are the sub-classes. 

  \code{\link{preview}} display a part of the table
}

\keyword{classes}
\keyword{database}