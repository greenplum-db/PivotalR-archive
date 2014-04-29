\name{madlib.elnet}
\alias{madlib.elnet}

\title{
  MADlib's elastic net regularization for generalized linear models
}

\description{
  This function wraps MAdlib's elastic net regularization for generalized linear models. Currently linear and logistic regressions are supported.
}
\usage{
madlib.elnet(formula, data, family = c("gaussian", "linear", "binomial",
"logistic"), na.action = NULL, na.as.level = FALSE, alpha = 1, lambda = 0.1,
standardize = TRUE, method = c("fista", "igd", "sgd", "cd"),
control = list(), glmnet = FALSE, ...)
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
    A string which indicates which form of regression to apply. Default value is "gaussian". The accepted values are: "gaussian" or "linear": Linear regression; "binomial" or "logistic": Logistic regression. The support for other families will be added in the future.
}
\item{na.action}{
          A string which indicates what should happen when the data
        contain \code{NA}s.  Possible
        values include \code{\link{na.omit}}, \code{"na.exclude"},
        \code{"na.fail"}
        and \code{NULL}. Right now, \code{\link{na.omit}} has been implemented. When the value is \code{NULL}, nothing is done on the R side and \code{NA} values are filtered on the MADlib side. User defined \code{na.action} function is allowed.
}

  \item{na.as.level}{
      A logical value, default is \code{FALSE}. Whether to treat \code{NA}
      value as a level in a categorical variable or just ignore it.
  }

\item{alpha}{
  A numeric value in [0,1], elastic net mixing parameter. The
          penalty is defined as

                     (1-alpha)/2||beta||_2^2+alpha||beta||_1.

          'alpha=1' is the lasso penalty, and 'alpha=0' the ridge
          penalty.

}
  \item{lambda}{
    A positive numeric value, the regularization parameter.
}
  \item{standardize}{
    A logical, default: \code{TRUE}. Whether to normalize the data. Setting this to \code{TRUE} usually yields better results and faster convergence.
}
  \item{method}{
    A string, default: "fista". Name of optimizer, "fista", "igd"/"sgd" or "cd". "fista" means the fast iterative shrinkage-thresholding algorithm [1], and "sgd" implements the stochastic gradient descent algorithm [2]. "cd" implements the coordinate descent algorithm [5].
}
  \item{control}{
    A list, which contains the control parameters for the optimizers.

    (1) If \code{method} is "fista", the allowed control parameters are:

    - \code{max.stepsize}
A numeric value, default is 4.0. Initial backtracking step size. At each iteration, the algorithm first tries stepsize = max.stepsize, and if it does not work out, it then tries a smaller step size, stepsize = stepsize/eta, where eta must be larger than 1. At first glance, this seems to perform repeated iterations for even one step, but using a larger step size actually greatly increases the computation speed and minimizes the total number of iterations. A careful choice of max_stepsize can decrease the computation time by more than 10 times.

- \code{eta}
A numeric value, default is 2. If stepsize does not work stepsize / eta is tried. Must be greater than 1.

- \code{use.active.set}
A logical value, default is FALSE. If use_active_set is TRUE, an active-set method is used to speed up the computation. Considerable speedup is obtained by organizing the iterations around the active set of features -- those with nonzero coefficients. After a complete cycle through all the variables, we iterate on only the active set until convergence. If another complete cycle does not change the active set, we are done, otherwise the process is repeated.

- \code{activeset.tolerance}
A numeric value, default is the value of the tolerance argument (see below). The value of tolerance used during active set calculation.

- \code{random.stepsize}
A logical value, default is \code{FALSE}. Whether to add some randomness to the step size. Sometimes, this can speed up the calculation.

(2) If \code{method} is "sgd", the allowed control parameters are:

- \code{stepsize}
The default is 0.01.

- \code{step.decay}
A numeric value, the actual setpsize used for current step is (previous stepsize) / exp(setp.decay). The default value is 0, which means that a constant stepsize is used in SGD.

- \code{threshold}
A numeric value, default is 1e-10. When a coefficient is really small, set this coefficient to be 0.
Due to the stochastic nature of SGD, we can only obtain very small values for the fitting coefficients. Therefore, threshold is needed at the end of the computation to screen out tiny values and hard-set them to zeros. This is accomplished as follows: (1) multiply each coefficient with the standard deviation of the corresponding feature; (2) compute the average of absolute values of re-scaled coefficients; (3) divide each rescaled coefficient with the average, and if the resulting absolute value is smaller than threshold, set the original coefficient to zero.

- \code{parallel}
 A logical value, the default is \code{True}. Whether to run the computation on multiple segments.
SGD is a sequential algorithm in nature. When running in a distributed manner, each segment of the data runs its own SGD model and then the models are averaged to get a model for each iteration. This averaging might slow down the convergence speed, although we also acquire the ability to process large datasets on multiple machines. This algorithm, therefore, provides the parallel option to allow you to choose whether to do parallel computation.

(3) The common control parameters for both "fista" and "sgd" optimizers:

- \code{max.iter} An integer, default is 100. The maximum number of iterations that are allowed.

- \code{tolerance} A numeric value, default is 1e-4. The criteria to end iterations. Basically 1e-4 will produce results with 4 significant digits. Both the "fista" and "sgd" optimizers compute the average difference between the coefficients of two consecutive iterations, and when the difference is smaller than tolerance or the iteration number is larger than max_iter, the computation stops.

- \code{warmup}
A logical value, default is \code{FALSE}. If warmup is \code{TRUE}, a series of lambda values, which is strictly descent and ends at the lambda value that the user wants to calculate, is used. The larger lambda gives very sparse solution, and the sparse solution again is used as the initial guess for the next lambda's solution, which speeds up the computation for the next lambda. For larger data sets, this can sometimes accelerate the whole computation and may be faster than computation on only one lambda value.

- \code{warmup.lambdas}
A vector of numeric values, default is \code{NULL}. The lambda value series to use when warmup is True. The default is \code{NULL}, which means that lambda values will be automatically generated. The warmup lambda values start from a large value and end at the \code{lambda} value.

- \code{warmup.lambda.no}
An integer Default: 15. How many lambdas are used in warm-up. If warmup_lambdas is not NULL, this value is overridden by the number of provided lambda values.

- \code{warmup.tolerance}
A numeric value, the value of tolerance used during warmup. The default is the same as the tolerance value.

(4) The control parameters for "cd" optimizer include

\code{max.iter}, \code{tolerance}, \code{use.active.set} and \code{verbose} for \code{family = "gaussian"}

\code{max.iter}, \code{tolerance}, \code{use.active.set}, \code{verbose}, \code{warmup}, \code{warmup.lambda.no} for \code{family   = "binomial"}

All parameters have been explained above. The only one left is \code{verbose}.

\code{verbose} A logical value, whether to output the warning message for "cd" optimizer. See the note section for details.
}
  \item{glmnet}{
    A logical value, default is \code{FALSE}. The R package \code{glmnet} states that "Note also that for gaussian, glmnet standardizes y to have unit variance before computing its lambda sequence (and then unstandardizes the resulting coefficients); if you wish to reproduce/compare results with other software, best to supply a standardized y." So if the user wants to compare the result of this function with that of \code{glmnet}, he can set this value to be \code{TRUE}, which tells this function to do the same data transformation as \code{glmnet} in the "gaussian" case so that one can easily compare the results. When \code{family = "binomial"}, this parameter is ignored.
}
  \item{\dots}{
    More arguments, currently not implemented.
  }
}

\note{
  The coordinate descent (\code{method = "cd"}) algorithm is currently only available in PivotalR. In the future, we will also implement it in MADlib. The idea is to do some part of the computation in memory. Due to the memory usage liimitation of the database, this method cannot handle the fitting where the number of features is too large (a couple of thousands).
}

\details{
  the objective function for
     \code{"gaussian"} is

                       1/2  RSS/nobs + lambda*penalty,

     and for the other models it is

                       -loglik/nobs + lambda*penalty.
}

\value{
  An object of \code{elnet.madlib} class, which is actually a list that contains the following items:

  \item{coef}{
    A vector, the fitting coefficients.
  }

  \item{intercept}{
    A numeric value, the intercept.
  }

  \item{y.scl}{
    A numeric value, which is used to scale the dependent values. In the "gaussian" case, it is 1 if \code{glmnet} is \code{FALSE}, and it is the standard deviation of the dependent variable if \code{glmnet} is \code{TRUE}.
  }

  \item{loglik}{
    A numeric value, the log-likelihood of the fitting result.
  }

  \item{standardize}{
    The \code{standardize} value in the arguments.
  }

  \item{iter}{
    An integer, the itertion number used.
  }

  \item{ind.str}{
    A string. The independent variables in an array format string.
  }

  \item{terms}{
    A \code{\link{terms}} object, describing the terms in the model formula.
  }

  \item{model}{
    A \code{\linkS4class{db.data.frame}} object, which wraps the result
    table of this function. When \code{method = "cd"}, there is no result table, because all the results are in R side.
  }

  \item{call}{
    A language object. The function call that generates this result.
  }

  \item{alpha}{
    The \code{alpha} in the arguments.
  }

  \item{lambda}{
    The \code{lambda} in the arguments.
  }

  \item{method}{
    The \code{method} string in the arguments.
  }

  \item{family}{
    The \code{family} string in the arguments.
  }

   \item{appear}{
    An array of strings, the same length as the number of independent
    variables. The strings are used to print a clean result, especially when
    we are dealing with the factor variables, where the dummy variable
    names can be very long due to the inserting of a random string to
    avoid naming conflicts, see \code{\link{as.factor,db.obj-method}}
    for details. The list also contains \code{dummy} and \code{dummy.expr}, which are also used for processing the categorical variables, but do not contain any important information.
  }

  \item{max.iter, tolerance}{
    The \code{max.iter} and \code{tolerance} in the \code{control}.
  }
}

\references{
[1] Beck, A. and M. Teboulle (2009), A fast iterative shrinkage-thresholding algorithm for linear inverse problems. SIAM J. on Imaging Sciences 2(1), 183-202.

[2] Shai Shalev-Shwartz and Ambuj Tewari, Stochastic Methods for l1 Regularized Loss Minimization. Proceedings of the 26th International Conference on Machine Learning, Montreal, Canada, 2009.

[3] Elastic net regularization. http://en.wikipedia.org/wiki/Elastic_net_regularization

[4] Kevin P. Murphy, Machine Learning: A Probabilistic Perspective, The MIT Press, Chap 13.4, 2012.

[5] Jerome Friedman, Trevor Hastie and Rob Tibshirani, Regularization Paths for Generalized Linear Models via Coordinate Descent, Journal of Statistical Software, Vol. 33(1), 2010.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Hai Qian, Pivotal Inc. \email{hqian@gopivotal.com}
}

\note{
  It is strongly recommended that you run this function on a subset of the data with a limited max_iter before applying it to the full data set with a large max.iter if the data set is big. In the pre-run, you can adjust the parameters to get the best performance and then apply the best set of parameters to the whole data set.
}

\seealso{
  \code{\link{generic.cv}} does k-fold cross-validation. See the examples there about how to use elastic net together with cross-validation.
}
\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

x <- matrix(rnorm(100*20),100,20)
y <- rnorm(100, 0.1, 2)

dat <- data.frame(x, y)

delete("eldata")
z <- as.db.data.frame(dat, "eldata", conn.id = cid, verbose = FALSE)

fit <- madlib.elnet(y ~ ., data = z, alpha = 0.2, lambda = 0.05, control
= list(random.stepsize=TRUE))

fit

lk(mean((z$y - predict(fit, z))^2)) # mean square error

fit <- madlib.elnet(y ~ ., data = z, alpha = 0.2, lambda = 0.05, method = "cd")

fit

db.disconnect(cid, verbose = FALSE)
}
}

\keyword{madlib}
\keyword{stats}
