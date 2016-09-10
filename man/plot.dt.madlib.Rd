\name{plot.dt.madlib}
\alias{plot.dt.madlib}

\title{
    Plot the result of madlib.rpart
}
\description{
  This is a visualization function which plots the result of \code{\link{madlib.rpart}}. This
  function internally calls R's \code{\link{plot.rpart}} function. }

\usage{

    \method{plot}{dt.madlib}(x, uniform = FALSE, branch = 1, compress = FALSE,
        nspace, margin = 0, minbranch = 0.3, ...)

    }

\arguments{
  \item{x}{
    The fitted tree from the result of \code{\link{madlib.rpart}}
}
  \item{uniform}{
    A boolean, if TRUE, uses uniform vertical spacing of the nodes.
}
  \item{branch}{
    A double value, between 0 and 1, to control the shape of the branches
    from parent to child.
}
  \item{compress}{
    A boolean, if FALSE, the leaf nodes will be at the horizontal plot coordinate of
    1:nleaves. Use TRUE for a more compact arrangement.
}
  \item{nspace}{
    A double value, indicating the amount of extra space between a node with children
    and a leaf. default is \code{branch}
}
  \item{margin}{
    A double value, indicating the amount of extra space to leave around the borders of
    the tree.
}
  \item{minbranch}{
    A double value, specifying the minimum length for a branch.
}
  \item{\dots}{
    Arguments to be passed to or from other methods.
}
}
\value{
  The coordinates of the nodes are returned as a list, with components x and y.
}
\references{
[1] Documentation of decision tree in MADlib 1.6, \url{http://doc.madlib.net/latest/}
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\seealso{
  \code{\link{madlib.rpart}} is the wrapper for MADlib's tree_train function for decision trees.
  \code{\link{text.dt.madlib}}, \code{\link{print.dt.madlib}} are other visualization functions.

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
fit

plot(fit, uniform =TRUE)
text(fit)

db.disconnect(cid)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
