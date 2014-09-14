\name{madlib.rpart}
\alias{madlib.rpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MADlib wrapper function for Decision Tree
}
\description{
    This function is a wrapper of MADlib's decision tree model training
    function. The resulting tree is stored in a table in the database, and one
    can also view the result from R using \code{\link{plot.dt.madlib}},
    \code{\link{text.dt.madlib}} and \code{\link{print.dt.madlib}}.
}
\usage{
madlib.rpart(formula, data, weights = NULL, id = NULL, na.action = NULL, parms,
control, na.as.level = FALSE, verbose = FALSE, ...) }
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
    A list, which includes parameters for the splitting function. Supported parameters 
    include:
    'split' specifying which split function to use. Options are 'gini', 'misclssification'
    and 'entropy' for classification, and 'mse' for regression. Default is 'gini' for
    classification and 'mse' for regression.
}
  \item{control}{
    A list, which includes parameters for the fit. Supported parameters include:
    'minsplit' - minimum number of observations that must be present in a node for a 
    split to be attempted. default is minsplit=20
    
    'minbucket' - Minimum number of observations in any terminal node, 
    default is min_split/3
    
    'maxdepth' - Maximum depth of any node, default is maxdepth=10 
    
    'n_bins' - Number of bins to find possible node split threshold values for continuous                   
               variables, default is 100 (Must be greater than 1) 
    
    'cp' - Cost complexity parameter, default is cp=0.01
    
}
  \item{na.as.level}{
    A boolean, indicating if NULL value for a categorical variable is treated as a 
    distinct level, default is na.as.level=false
}
  \item{verbose}{
    A boolean, indicating whether or not to print more info, default is verbose=false
}
  \item{\dots}{
    Arguments to be passed to or from other methods.
}
}
\value{
  An S3 object of type dt.madlib in the case of non-grouping, and of type
  dt.madlib.grp in the case of grouping.
}
\references{
[1] Documentation of decision tree in MADlib 1.6, \url{http://doc.madlib.net/latest/}
}
\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@pivotal.io}  
}

\seealso{ 
  \code{\link{plot.dt.madlib}}, \code{\link{text.dt.madlib}}, \code{\link{print.dt.madlib}} are 
  visualization functions for a model fitted through madlib.rpart 
  
  \code{\link{predict.dt.madlib}} is a wrapper for MADlib's predict function for 
  decision trees.

  \code{\link{madlib.lm}}, \code{\link{madlib.glm}},
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
key(x) <- "id"
fit <- madlib.rpart(rings < 10 ~ length + diameter + height + whole + shell, 
       data=x, parms = list(split='gini'), control = list(cp=0.005))
fit

## Another example, using grouping  
fit <- madlib.rpart(rings < 10 ~ length + diameter + height + whole + shell | sex, 
       data=x, parms = list(split='gini'), control = list(cp=0.005))
fit

db.disconnect(cid)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
