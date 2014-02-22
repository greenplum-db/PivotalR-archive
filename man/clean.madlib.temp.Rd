\name{clean.madlib.temp}
\alias{clean.madlib.temp}

\title{
  Delete all the result tables created during calculations of
  MADlib
}
\description{
  Some MADlib wrapper functions create result tables that cannot be
  dropped in the background, because other functions need to use these
  tables. For example, \code{\link{madlib.arima}} creates 3 result
  tables, which are needed by
  \code{\link{predict.arima.css.madlib}}. One can manually delete these
  3 tables when they are not useful anymore using
  \code{\link{delete,arima.css.madlib-method}}. One can also choose to
  all such tables created by many such functions together using this
  function.
}

\usage{
clean.madlib.temp(conn.id = 1)
}

\arguments{
  \item{conn.id}{
    An integer, the connection ID of the database. See
  \code{\link{db.connect}} for more details.
  }
}

\details{
  All such result tables created by MADlib wrapper functions start with
  "__madlib_temp_" followed by three random integers. This function
  deletes all such tables.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\seealso{
  \code{\link{madlib.arima}} creates three tables with names starting
  with "__madlib_temp_" when it fits ARIMA model to time series

  \code{\link{delete,arima.css.madlib-method}} deletes the result of
  \code{\link{madlib.arima}} together with the model, residual and statistics
  tables.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## use double values as the time stamp
## Any values that can be ordered will work
example_time_series <- data.frame(
    id = seq(0,1000,length.out=length(ts)),
    val = arima.sim(list(order=c(2,0,1),
    ar=c(0.7, -0.3), ma=0.2), n=1000000) + 3.2)

x <- as.db.data.frame(example_time_series, field.types =
                      list(id="double precision", val = "double precision"),
                      conn.id = cid, verbose = FALSE)

dim(x)

names(x)

## use formula
s <- madlib.arima(val ~ id, x, order = c(2,0,1))

s

## delete all result tables
clean.madlib.temp(conn.id = 1)

## s still exists but the 3 tables (model, residuals, etc.) are deleted
s

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{utility}
\keyword{database}
