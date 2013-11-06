\name{madlib.elnet}
\alias{madlib.elnet}

\title{
  MADlib's elastic net regularization for generalized linear models
}

\description{
  This function wraps MAdlib's elastic net regularization for generalized linear models. Currently linear and logistic regressions are supported.  
}
\usage{
madlib.elnet(formula, data, family = "gaussian", na.action, alpha = 1, lambda = 0.1, standardize = TRUE, method = "fista", control = list(), glmnet = FALSE, ...)
}

\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{na.action}{
%%     ~~Describe \code{na.action} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
}
  \item{standardize}{
%%     ~~Describe \code{standardize} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{control}{
%%     ~~Describe \code{control} here~~
}
  \item{glmnet}{
%%     ~~Describe \code{glmnet} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
  
}

\references{
%% ~put references to the literature/web site here ~
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.  

  Maintainer: Hai Qian \email{hqian@gopivotal.com}
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
"madlib.elnet"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
