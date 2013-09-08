\name{madlib.arima}
\alias{madlib.arima}
\alias{madlib.arima-db.Rquery,db.Rquery-method}
\alias{madlib.arima-formula,db.obj-method}

\title{Wrapper for MADlib's ARIMA model fitting function}

\description{
  Apply ARIM model fitting onto a table that contains time series
  data. The table must have two columns: one for the time series values,
  and the other for the time stamps. The time stamp can be anything that
  can be ordered. This is because the rows of a table does not have
  inherent order and thus needs to be ordered by the extra time stamp
  column. 
}

\usage{
\S4method{madlib.arima}{db.Rquery,db.Rquery}(x, ts, by = NULL,
order=c(1,1,1), seasonal = list(order = c(0,0,0), period = NA),
include.mean = TRUE, method = "CSS", optim.method = "LM",
optim.control = list(), ...)

\S4method{madlib.arima}{formula,db.obj}(x, ts, order=c(1,1,1),
seasonal = list(order = c(0,0,0), period = NA), include.mean = TRUE,
method = "CSS", optim.method = "LM", optim.control = list(), ...)
}

\arguments{
  \item{x}{

  }

  \item{ts}{

  }

  \item{by}{

  }

  \item{order}{

  }

  \item{seasonal}{

  }

  \item{include.mean}{

  }

  \item{method}{

  }

  \item{optim.method}{

  }

  \item{optim.control}{

  }
}

\details{

}

\value{
  \item{coef}{

  }

  \item{s.e.}{

  }

  \item{series}{

  }

  \item{time.stamp}{

  }

  \item{time.series}{

  }

  \item{sigma2}{

  }

  \item{loglik}{

  }

  \item{iter.num}{

  }

  \item{exec.time}{

  }

  \item{residuals}{

  }

  \item{model}{

  }

  \item{statistics}{

  }

  \item{call}{

  }
}

\references{

}

\author{
  Author: Predictive Analytics Team at Pivotal Inc. \email{user@madlib.net}

  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics
  Team at Pivotal Inc. \email{user@madlib.net}, Hai Qian
  \email{hai.qian@gopivotal.com} 
}

\seealso{
  
}

\examples{
\dontrun{

}
}