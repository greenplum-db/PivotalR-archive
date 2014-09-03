\name{predict.dt.madlib}
\alias{predict.dt.madlib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Compute the predictions of the model produced by madlib.rpart
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.dt.madlib(object, newdata, type = c("response", "prob"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (object, newdata, type = c("response", "prob")) 
{
    type <- match.arg(type)
    if (missing(newdata)) 
        newdata <- object$data
    if (is(newdata, "db.Rquery")) {
        newdata <- as.db.data.frame(newdata)
        is.temp <- TRUE
    }
    else {
        is.temp <- FALSE
    }
    conn.id <- conn.id(newdata)
    tbl.predict <- .unique.string()
    madlib <- schema.madlib(conn.id)
    sql = paste("select ", madlib, ".tree_predict('", content(object), 
        "', '", content(newdata), "', '", tbl.predict, "'", type, 
        "')", sep = "")
    .db(sql, conn.id = conn.id, verbose = FALSE)
    if (is.temp) 
        delete(newdata)
    db.data.frame(tbl.predict, conn.id = conn.id, verbose = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
