\name{print.none.obj}
\alias{print.none.obj}

\title{Function used in GUI to print absolutely nothing}

\description{
  This function prints nothing and is used only in GUI.
}

\usage{
\method{print}{none.obj}(x, ...)
}

\arguments{
  \item{x}{
    A \code{none.obj} object. The content of this object does not
    matter. It is used to return a value which makes the GUI print
    nothing on the screen.
  }

  \item{\dots}{
    Not used.
  }
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\seealso{
    \code{\link{PivotalR}} launches the GUI for PivotalR.
}

\keyword{IO}
\keyword{GUI}
\keyword{utilities}
