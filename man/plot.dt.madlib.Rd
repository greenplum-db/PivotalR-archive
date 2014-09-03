\name{plot.dt.madlib}
\alias{plot.dt.madlib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Plot the result of madlib.rpart
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.dt.madlib(x, uniform = FALSE, branch = 1, compress = FALSE, nspace, margin = 0, minbranch = 0.3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{uniform}{
%%     ~~Describe \code{uniform} here~~
}
  \item{branch}{
%%     ~~Describe \code{branch} here~~
}
  \item{compress}{
%%     ~~Describe \code{compress} here~~
}
  \item{nspace}{
%%     ~~Describe \code{nspace} here~~
}
  \item{margin}{
%%     ~~Describe \code{margin} here~~
}
  \item{minbranch}{
%%     ~~Describe \code{minbranch} here~~
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

## The function is currently defined as
function (x, uniform = FALSE, branch = 1, compress = FALSE, nspace, 
    margin = 0, minbranch = 0.3, ...) 
{
    library(rpart)
    class(x) <- "rpart"
    plot(x, uniform = uniform, branch = branch, compress = compress, 
        nspace = nspace, margin = margin, minbranch = minbranch, 
        ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
