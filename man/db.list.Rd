\name{db.list}
\alias{db.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ++function to do ... ++
}
\description{
  ++ A concise (1-5 lines) description of what the function does. ++
}
\usage{
db.list()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    n.conn <- length(.localVars$db)
    cat("Database Connection Info\n")
    for (i in seq(n.conn)) {
        cat("\n## -------------------------------\n")
        cat(paste("[Connection ID ", i, "]\n", sep = ""))
        cat(paste("Host :     ", .localVars$db[[i]]$host, "\n", 
            sep = ""))
        cat(paste("User :     ", .localVars$db[[i]]$user, "\n", 
            sep = ""))
        cat(paste("Database : ", .localVars$db[[i]]$dbname, "\n", 
            sep = ""))
        cat(paste("Conn pkg : ", .localVars$db[[i]]$conn.pkg, 
            "\n", sep = ""))
    }
  }

## End (Not run)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
