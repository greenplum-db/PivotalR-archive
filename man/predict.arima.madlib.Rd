\name{predict.arima}
\alias{predict.arima.css.madlib}

\title{Forecast from MADlib's ARIMA fits}

\description{Forecast from models fit by \code{linlk{madlib.arima}}}

\usage{
\method{predict}{arima.css.madlib}(object, n.ahead = 1, ...)
}

\arguments{
  \item{object}{
    The result of \code{\link{madlib.arima}}.
  }

  \item{n.ahead}{
    The number of steps ahead for which prediction is required.
  }

  \item{\dots}{
    Arguments passed to or from other methods, not implemented yet.
  }
}

\value{
  A \code{\linkS4class{db.table}} object, which points to a table that
  contains the forecasted values. The table has two columns: steps_ahead
  and forecast_value. One can use the function \code{\link{lk}} to look at the values.
}

\author{
 Author: Predictive Analytics Team at Pivotal Inc.
  
  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@pivotal.io}
}

\seealso{
  \code{\link{madlib.arima}} fits ARIMA model to a time series.
}

\examples{
\dontrun{
## Please see the examples in madlib.arima doc.
}
}

\keyword{madlib}
\keyword{stats}
\keyword{math}
