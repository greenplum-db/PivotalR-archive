\name{madlib.arima}
\alias{madlib.arima}
\alias{madlib.arima,db.Rquery,db.Rquery-method}
\alias{madlib.arima,formula,db.obj-method}

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
    A formula with the format of \code{time series value} \code{~}
  \code{time stamp} \code{|} \code{grouping col_1 + ... + grouping
  col_n}. Or a \code{\linkS4class{db.Rquery}} object, which is the
\code{time series value}. Grouping is not implemented yet. Both time
  stamp and time series can be valid expressions.

  We must specify the time stamp because the table in database has no
  order of rows, and we have to order they according the given time
  stamps.
  }

  \item{ts}{
    If \code{x} is a formula object, this must be a
    \code{\linkS4class{db.obj}} object, which contains both the time
    series and time stamp columns. If \code{x} is a
    \code{\linkS4class{db.Rquery}} object, this must be another
    \code{db.Rquery} object, which is the \code{time stamp} and can be a
    valid expression.
  }

  \item{by}{
    A list of \code{\linkS4class{db.Rquery}}, the default is NULL. The
    grouping columns. Right now, this functionality is not implemented
    yet.
  }

  \item{order}{
    A vector of 3 integers, default is \code{c(1,1,1)}. The ARIMA orders
    \code{p, d, q} for AR, I and MA.
  }

  \item{seasonal}{
    A list of \code{order} and \code{perid}, default is \code{list(order
    = c(0,0,0), period = NA)}. The seasonal orders and period. Currently
    not implemented.
  }

  \item{include.mean}{
    A logical value, default is \code{TRUE}. Whether to estimate the
    mean value of the time series. If the integration order \code{d}
    (the second element of order) is not zero, \code{include.mean} is
    set to \code{FALSE} in the calculation.
  }

  \item{method}{
    A string, the fitting method. The default is "CSS", which uses
    conditional-sum-of-squares to fit the time series. Right now, only
    "CSS" is supported.
  }

  \item{optim.method}{
    A string, the optimization method. The default is "LM", the
    Levenberg-Marquardt algorithm. Right now, only "LM" is supported.
  }

  \item{optim.control}{
    A list, default is \code{list()}. The control parameters of the
    optimizer. For \code{optim.method="LM"}, it can have the following
    optional parameters:

    - max_iter: Maximum number of iterations to run learning algorithm
    (Default = 100)

    - tau: Computes the initial step size for gradient algorithm
    (Default = 0.001)

    - e1: Algorithm-specific threshold for convergence (Default = 1e-15)

    - e2: Algorithm-specific threshold for convergence (Default = 1e-15)

    - e3: Algorithm-specific threshold for convergence (Default = 1e-15)

    - hessian_delta: Delta parameter to compute a numerical
    approximation of the Hessian matrix (Default = 1e-6)
  }

  \item{\dots}{
    Other optional parameters. Not implemented.
  }
}

\details{
  Given a time series of data X, the Autoregressive Integrated Moving
  Average (ARIMA) model is a tool for understanding and, perhaps,
  predicting future values in the series. The model consists of three
  parts, an autoregressive (AR) part, a moving average (MA) part, and an
  integrated (I) part where an initial differencing step can be applied
  to remove any non-stationarity in the signal. The model is generally
  referred to as an ARIMA(p, d, q) model where parameters p, d, and q
  are non-negative integers that refer to the order of the
  autoregressive, integrated, and moving average parts of the model
  respectively.

  MADlib's ARIMA function implements a parallel version of the LM
  algorithm to maximize the conditional log-likelihood, which is
  suitable for big data.
}

\value{
  Returns an \code{arima.css.madlib} object, which is a list that
  contains the following items:

  \item{coef}{
    A vector of double values. The fitting coefficients of AR, MA and
    mean value (if \code{include.mean} is \code{TRUE}).
  }

  \item{s.e.}{
    A vector of double values. The standard errors of the fitting
    coefficients.
  }

  \item{series}{
    A string, the data source table or SQL query.
  }

  \item{time.stamp}{
    A string, the name of the time stamp column.
  }

  \item{time.series}{
    A string, the name of the time series column.
  }

  \item{sigma2}{
    the MLE of the innovations variance.
  }

  \item{loglik}{
    the maximized conditional log-likelihood (of the differenced data).
  }

  \item{iter.num}{
    An integer, how many iterations of the LM algorithm is used to fit
    the time series with ARIMA model.
  }

  \item{exec.time}{
    The time spent on the MADlib ARIMA fitting.
  }

  \item{residuals}{
    A \code{\linkS4class{db.data.frame}} object that points to the table
    that contains all the fitted innovations.
  }

  \item{model}{
     A \code{\linkS4class{db.data.frame}} object that points to the
     table that contains the coefficients and standard error. This table
     is needed by \code{\link{predict.arima.css.madlib}}.
  }

  \item{statistics}{
    A \code{\linkS4class{db.data.frame}} object that points to the
    table that contains information including log-likelihood, sigma^2
    etc. This table is needed by
    \code{\link{predict.arima.css.madlib}}.
  }

  \item{call}{
    A language object. The matched function call.
  }
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\references{
  [1] Rob J Hyndman and George Athanasopoulos: Forecasting: principles
  and practice, http://otexts.com/fpp/

  [2] Robert H. Shumway, David S. Stoffer: Time Series Analysis and Its
  Applications With R Examples, Third edition Springer Texts in
  Statistics, 2010

  [3] Henri Gavin: The Levenberg-Marquardt method for nonlinear least
  squares curve-fitting problems, 2011
}

\seealso{
    \code{\link{madlib.lm}}, \code{\link{madlib.glm}},
  \code{\link{madlib.summary}} are MADlib
  wrapper functions.

  \code{\link{delete}} deletes the result of
  this function together with the model, residual and statistics
  tables.

  \code{\link{print.arima.css.madlib}}, \code{\link{show.arima.css.madlib}} and
  \code{\link{summary.arima.css.madlib}} prints the result in a pretty
  format.

  \code{predict.arima.css.madlib} makes forecast of the time series
  based upon the result of this function.
}

\examples{
\dontrun{
library(PivotalR)
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## use double values as the time stamp
## Any values that can be ordered will work
example_time_series <- data.frame(id =
                       seq(0,1000,length.out=length(ts)),
                       val = arima.sim(list(order=c(2,0,1), ar=c(0.7,
                             -0.3), ma=0.2), n=1000000) + 3.2)

x <- as.db.data.frame(example_time_series, field.types = list(id="double
     precision", val = "double precision"), conn.id = cid)

dim(x)

names(x)

## use formula
s <- madlib.arima(val ~ id, x, order = c(2,0,1))

s

## delete s and the 3 tables: model, residuals and statistics
delete(s)

s # s does not exist any more

## do not use formula
s <- madlib.arima(x$val, x$id, order = c(2,0,1))

s

lookat(sort(s$residuals, F, s$residuals$tstamp), 10)

lookat(s$model)

lookat(s$statistics)

## 10 forecasts
pred <- predict(s, n.ahead = 10)

lookat(sort(pred, F, pred$step_ahead), "all")

## Use expressions
s <- madlib.arima(val+2 ~ I(id + 1), x, order = c(2,0,1))

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{madlib}
\keyword{stats}