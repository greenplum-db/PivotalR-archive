\name{null.data}

\alias{null.data}

\docType{data}

\title{
  A Data Set with lots of \code{NA} values
}

\description{
  An example \code{data.frame} which is used by examples in this user manual
}

\usage{data(null.data)}

\format{
This data has 104 columns and 2000 rows.
}

\note{Lazy data loading is enabled in this package. So the user does not
need to explicitly run \code{data(null.data)} to load the data. It will be
loaded whenever it is used.}

\details{
  This data set has lots of \code{NA} values in it. By using
  \code{\link{as.db.data.frame}}, one can put the data set into the
  connected database. All the \code{NA} values will be converted into
  \code{NULL} values.

  The MADlib wrapper functions like \code{\link{madlib.lm}} and
  \code{link{madlib.glm}} will throw an error if there are \code{NULL}
  values in the data. So one needs to clean up the data before using the
  regression functions supplied by MADlib.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

## create a table from the example data.frame "abalone"
delete("null_data", conn.id = cid)
x <- as.db.data.frame(null.data, "null_data", conn.id = cid, verbose = FALSE)

## ERROR, because of NULL values
fit <- madlib.lm(sf_mrtg_pct_assets ~ ris_asset + lncrcd + lnauto +
                 lnconoth + lnconrp + intmsrfv + lnrenr1a + lnrenr2a +
                 lnrenr3a, data = x)

## select columns
y <- x[,c("sf_mrtg_pct_assets","ris_asset", "lncrcd","lnauto",
          "lnconoth","lnconrp","intmsrfv","lnrenr1a","lnrenr2a",
          "lnrenr3a")]

dim(y)

## remove NULL values
for (i in 1:10) y <- y[!is.na(y[i]),]

dim(y)

fit <- madlib.lm(sf_mrtg_pct_assets ~ ., data = y)

fit

db.disconnect(cid, verbose = FALSE)
}
}
\keyword{database}
\keyword{data operation}
