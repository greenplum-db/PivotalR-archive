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

\value{
    No value is returned
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}

\seealso{
    \code{\link{PivotalR}} launches the GUI for PivotalR.
}

\keyword{IO}
\keyword{GUI}
\keyword{utilities}