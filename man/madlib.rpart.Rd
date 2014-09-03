\name{madlib.rpart}
\alias{madlib.rpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MADlib wrapper functino for Decision Tree
}
\description{
    This function is a wrapper of MADlib's decision tree model training
    function. The resulting tree is stored in a table in the database, and one
    can also view the result from R using \code{\link{plot.dt.madlib}},
    \code{\link{text.dt.madlib}} and \code{\link{print.dt.madlib}}.
}
\usage{
madlib.rpart(formula, data, weights = NULL, id = NULL, na.action = NULL, parms, control, na.as.level = FALSE, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
      A formula object, intercept term will automatically be removed. Factors will
      not be expanded to their dummy variables. Grouping syntax is also supported,
      see \code{\link{madlib.lm}} and \code{\link{madlib.glm}} for more details.
}
  \item{data}{
    A \code{\linkS4class{db.obj}} object, which wraps the data in the database.
}
  \item{weights}{
    A string, the column name for the weights.
}
  \item{id}{
    A string, the index for each row. If \code{\link{key}} has been specified for
    \code{data}, teh key will be used as the ID unless this argument is also specified.
    We have to have this specified so that \code{\link{predict.dt.madlib}}'s result can
    be compared with the original data.
}
  \item{na.action}{
    A function, which filters the \code{NULL} values from the data. Not implemented yet.
}
  \item{parms}{
    A list, which includes
}
  \item{control}{
%%     ~~Describe \code{control} here~~
}
  \item{na.as.level}{
%%     ~~Describe \code{na.as.level} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
