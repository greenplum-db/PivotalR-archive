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
    A formula (or one that can be coerced to
    that class), specifies the dependent and independent variables.
}
  \item{data}{
    A \code{\linkS4class{db.obj}} object. Currently, this parameter is
  mandatory. If it is an object of class \code{db.Rquery} or
  \code{db.view}, a temporary table will be created, and further
  computation will be done on the temporary table. After the
  computation, the temporary will be dropped from the corresponding
  database.
}
  \item{family}{
    A string which indicates which form of regression to apply. Default value is "gaussian". The accepted values are: ``gaussian'' or ``linear'': Linear regression; ``binomial'' or ``logistic'': Logistic regression. The support for other families will be added in the future.
}
\item{na.action}{
          A string which indicates what should happen when the data
        contain \code{NA}s.  Future possible
        values include \code{"na.omit"}, \code{"na.exclude"},
        \code{"na.fail"}
        and \code{NULL}. Currently not implemented yet.
}
\item{alpha}{
  A value in [0,1], elastic net control parameter.
}
  \item{lambda}{
    A positive value, the regularization parameter.
}
  \item{standardize}{
    A logical, default: \code{TRUE}. Whether to normalize the data. Setting this to \code{TRUE} usually yields better results and faster convergence.
}
  \item{method}{
    A string, default: "fista". Name of optimizer, either "fista" or "igd".
}
  \item{control}{
    
}
  \item{glmnet}{
    
}
  \item{\dots}{
    More arguments, currently not implemented.
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

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
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
