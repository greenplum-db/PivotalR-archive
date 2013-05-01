\name{db.disconnect}
\alias{db.disconnect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  function to do ... 
}
\description{
   A concise (1-5 lines) description of what the function does. 
}
\usage{
db.disconnect(conn.id = 1, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn.id}{
     Describe \code{conn.id} here
   }
   \item{verbose}{
     verbose
   }
}
\details{
  ++ If necessary, more details than the description above ++
}
\value{
  +Describe the value returned
  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
 +put references to the literature/web site here +
}
\author{
  ++who you are++
}
\note{
  ++further notes++
}

%% +Make other sections like Warning with \section{Warning }{....} +

\seealso{
 ++objects to See Also as \code{\link{help}}, +++
}
\examples{
## Not run:
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (conn.id = 1) 
{
    if (!.is.conn.id.valid(conn.id)) 
        stop("There is no such connection!")
    conn.pkg <- .localVars$db[[conn.id]]$conn.pkg
    command <- paste(".db.disconnect.", conn.pkg, "(conn.id=", 
        conn.id, ")", sep = "")
    eval(parse(text = command))
    .localVars$db[[conn.id]] <- NULL
    .localVars$conn.type[[conn.pkg]] <- .localVars$conn.type[[conn.pkg]][-which(.localVars$conn.type[[conn.pkg]] == 
        conn.id)]
  }
## End (Not run)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
