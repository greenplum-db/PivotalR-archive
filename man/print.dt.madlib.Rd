\name{print.dt.madlib}
\alias{print.dt.madlib}

\title{
    Print the result of madlib.rpart
}
\description{
    This function prints the result of \code{\link{madlib.rpart}} to the screen. It
    internally calls R's \code{\link{print.rpart}} function.
}
\usage{
print.dt.madlib(x, digits = max(3L, getOption("digits") - 3L), ...)
}
\arguments{
  \item{x}{
    The fitted tree from the result of \code{\link{madlib.rpart}}
}
  \item{digits}{
    The number of digits to print for numerical values.
}
  \item{\dots}{
    Arguments to be passed to or from other methods.
}
}
\references{
[1] Documentation of decision tree in MADlib 1.6, \url{http://doc.madlib.net/latest/}
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc.
  
  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@pivotal.io}
}

\seealso{
  \code{\link{madlib.rpart}} is the wrapper for MADlib's tree_train function for decision trees.
  \code{\link{madlib.plot}}, \code{\link{madlib.text}} are other visualization functions.
  
  \code{\link{madlib.lm}}, \code{\link{madlib.glm}}, \code{\link{madlib.rpart}},
  \code{\link{madlib.summary}}, \code{\link{madlib.arima}}, \code{\link{madlib.elnet}}
  are all MADlib wrapper functions.
}

\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

x <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
lk(x, 10)

## decision tree using abalone data, using default values of minsplit, 
## maxdepth etc.
key(x)<-"id"
fit <- madlib.rpart(rings < 10 ~ length + diameter + height + whole + shell, 
       data=x, parms = list(split='gini'), control = list(cp=0.005))

print(fit)

db.disconnect(cid)   
}

## The function is currently defined as
function (x, digits = max(3L, getOption("digits") - 3L), ...) 
{
    library(rpart)
    class(x) <- "rpart"
    out <- capture.output(print(x))
    cat(paste(gsub(">=", ">", gsub("<", "<=", out)), collapse = "\n"), 
        "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
