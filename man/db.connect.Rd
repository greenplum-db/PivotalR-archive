\name{db.connect}
\alias{db.connect}
%% - Also NEED an '\alias' for EACH other topic documented here.
\title{
  ++function to do ... ++
}
\description{
  ++ A concise (1-5 lines) description of what the function does. ++
}
\usage{
db.connect(host, user, dbname, password = "", port = "", madlib =
"madlib", conn.pkg = "RPostgreSQL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{host}{
     ++Describe \code{host} here++
}
  \item{user}{
     ++Describe \code{user} here++
}
  \item{dbname}{
     ++Describe \code{dbname} here++
}
  \item{password}{
     ++Describe \code{password} here++
   }
   \item{port}{
     port number
   }
   \item{madlib}{
     madlib
   }
  \item{conn.pkg}{
     ++Describe \code{conn.pkg} here++
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
function (host, user, dbname, password = "", conn.pkg = "RPostgreSQL") 
{
    if (is.null(.localVars$installed.pkgs)) 
        .localVars$installed.pkgs <- tolower(attr(installed.packages(), 
            "dimnames")[[1]])
    if (!.is.arg.string(host) || !.is.arg.string(user) || !.is.arg.string(dbname) || 
        !.is.arg.string(password) || !.is.arg.string(conn.pkg)) 
        stop("Host, user, dbname, password (could be an empty string) and the connection package should all be strings!")
    conn.pkg.name <- tolower(conn.pkg)
    if (conn.pkg.name \%in\% tolower(.supported.connections)) {
        i <- which(tolower(.supported.connections) == conn.pkg.name)
        pkg.to.load <- .supported.connections[i]
        if (!(conn.pkg.name \%in\% .localVars$installed.pkgs)) {
            print(paste("Package ", pkg.to.load, " is going to be installed so that ", 
                .this.pkg.name, " could connect to databases.\n\n", 
                sep = ""))
            install.packages(pkgs = pkg.to.load, type = "source")
        }
        eval(parse(text = paste("library(", pkg.to.load, ")", 
            sep = "")))
        command <- paste(".db.connect.", conn.pkg.name, "(host=\"", 
            host, "\", user=\"", user, "\", dbname=\"", dbname, 
            "\", password=\"", password, "\")", sep = "")
        result <- eval(parse(text = command))
        return(paste("Created a connection to database with ID", 
            result))
    }
    else {
        stop(paste("Right now, only ", .supported.connections, 
            " is supported to connected to database.\n", sep = ""))
    }
  }

## End (Not run)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
