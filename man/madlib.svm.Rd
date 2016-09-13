\name{madlib.svm}
\alias{madlib.svm}

\title{
  Support Vector Machine with regression and novelty detection
}

\description{
  This function wraps MADlib's SVM for classification, regresssion and novelty detection.
}

\usage{
madlib.svm (formula, data,
            na.action = NULL, na.as.level = FALSE,
            type = c("classification", "regression", "one-class"),
            kernel = c("gaussian", "linear", "polynomial"),
            degree = 3, gamma = NULL, coef0 = 1.0, class.weight = NULL,
            tolerance = 1e-10, epsilon = NULL, cross = 0, lambda = 0.01,
            control = list(), verbose = FALSE, ...)
}

\arguments{
  \item{formula}{
    an object of class "\code{\link{formula}}" (or one that can be coerced to
    that class): a symbolic description of the model to be
    fitted. The details of model specification are given under
    'Details'.
  }

  \item{data}{
    An object of \code{db.obj} class. Currently, this parameter is
    mandatory. If it is an object of class \code{db.Rquery} or
    \code{db.view}, a temporary table will be created, and further
    computation will be done on the temporary table. After the
    computation, the temporary will be dropped from the corresponding
    database.
  }

  \item{na.action}{
    A string which indicates what should happen when the data
    contain \code{NA}s. Possible values include \code{\link{na.omit}}, \code{"na.exclude"}, \code{"na.fail"} and \code{NULL}. Right now, \code{\link{na.omit}} has been implemented. When the value is \code{NULL}, nothing is done on the R side and \code{NA} values are filtered on the MADlib side. User defined \code{na.action} function is allowed.
  }

  \item{na.as.level}{
    A logical value, default is \code{FALSE}. Whether to treat \code{NA}
    value as a level in a categorical variable or just ignore it.
  }

  \item{type}{
    A string, default: "classification". Indicate type of analysis to perform: "classification", "regression" or "one-class".
  }

  \item{kernel}{
    A string, default: "gaussian". Type of kernel. Currently three kernel types are supported: 'linear', 'gaussian', and 'polynomial'.
  }

  \item{degree}{
    Default: 3. The parameter needed for polynomial kernel
  }

  \item{gamma}{
    Default: 1/num_features. The parameter needed for gaussian kernel
  }

  \item{coef0}{
    Default: 1.0. The independent term in polynomial kernel
  }

  \item{class.weight}{
    Default: 1.0. Set the weight for the positive and negative classes. If not given, all classes are set to have weight one. If class_weight = balanced, values of y are automatically adjusted as inversely proportional to class frequencies in the input data i.e. the weights are set as n_samples / (n_classes * bincount(y)).

    Alternatively, class_weight can be a mapping, giving the weight for each class. Eg. For dependent variable values 'a' and 'b', the class_weight can be {a: 2, b: 3}. This would lead to each 'a' tuple's y value multiplied by 2 and each 'b' y value will be multiplied by 3.

    For regression, the class weights are always one.
  }

  \item{tolerance}{
    Default: 1e-10. The criterion to end iterations. The training stops whenever <the difference between the training models of two consecutive iterations is <smaller than tolerance or the iteration number is larger than max_iter.
  }

  \item{epsilon}{
    Default: [0.01]. Determines the epsilon for epsilon-SVR. Ignored during classification. When training the model, differences of less than epsilon between estimated labels and actual labels are ignored. A larger epsilon will yield a model with fewer support vectors, but will not generalize as well to future data. Generally, it has been suggested that epsilon should increase with noisier data, and decrease with the number of samples. See [5].
  }

  \item{cross}{
    Default: 0. Number of folds (k). Must be at least 2 to activate cross validation. If a value of k > 2 is specified, each fold is then used as a validation set once, while the other k - 1 folds form the training set.
  }

  \item{lambda}{
    Default: [0.01]. Regularization parameter. Must be non-negative.
  }

  \item{control}{
    A list, which contains the more control parameters for the optimizer.

    - \code{init.stepsize}: Default: [0.01]. Also known as the initial learning rate. A small value is usually desirable to ensure convergence, while a large value provides more room for progress during training. Since the best value depends on the condition number of the data, in practice one often searches in an exponential grid using built-in cross validation; e.g., "init_stepsize = [1, 0.1, 0.001]". To reduce training time, it is common to run cross validation on a subsampled dataset, since this usually provides a good estimate of the condition number of the whole dataset. Then the resulting init_stepsize can be run on the whole dataset.

    - \code{decay.factor}: Default: [0.9]. Control the learning rate schedule: 0 means constant rate; <-1 means inverse scaling, i.e., stepsize = init_stepsize / iteration; > 0 means <exponential decay, i.e., stepsize = init_stepsize * decay_factor^iteration.

    - \code{max.iter}: Default: [100]. The maximum number of iterations allowed.

    - \code{norm}: Default: 'L2'. Name of the regularization, either 'L2' or 'L1'.

    - \code{eps.table}: Default: NULL. Name of the input table that contains values of epsilon for different groups. Ignored when grouping_col is NULL. Define this input table if you want different epsilon values for different groups. The table consists of a column named epsilon which specifies the epsilon values, and one or more columns for grouping_col. Extra groups are ignored, and groups not present in this table will use the epsilon value specified in parameter epsilon.

    - \code{validation.result}: Default: NULL. Name of the table to store the cross validation results including the values of parameters and their averaged error values. For now, metric like 0-1 loss is used for classification and mean square error is used for regression. The table is only created if the name is not NULL.
  }

  \item{verbose}{
    A logical value, default: FALSE. Verbose output of the results of training.
  }

  \item{\dots}{
    More parameters can be passed into this function. Currently, it is
    just a place holder and any parameter here is not used.
  }
}

\details{
  For details about how to write a formula, see \code{\link{formula}}
  for details. "|" can be used at the end of the formula to denote that
  the fitting is done conditioned on the values of one or more
  variables. For example, \code{y ~ x + sin(z) | v + w} will do the
  fitting each distinct combination of the values of \code{v} and
  \code{w}.
}

\value{
  If there is no grouping (i.e. no \code{|} in the formula), the result
  is a \code{svm.madlib} object. Otherwise, it is a \code{svm.madlib.grps}
  object, which is just a list of \code{svm.madlib} objects.

  A \code{svm.madlib} object is a list which contains the following items:

  \item{coef}{
    A vector, the fitting coefficients.
  }

  \item{grps}{
    An integer, the number of groups that the data is divided into
    according to the grouping columns in the formula.
  }

  \item{grp.cols}{
    An array of strings. The column names of the grouping columns.
  }

  \item{has.intercept}{
    A logical, whether the intercept is included in the fitting.
  }

  \item{ind.vars}{
    An array of strings, all the different terms used as independent
    variables in the fitting.
  }

  \item{ind.str}{
    A string. The independent variables in an array format string.
  }

  \item{call}{
    A language object. The function call that generates this result.
  }

  \item{col.name}{
    An array of strings. The column names used in the fitting.
  }

  \item{appear}{
    An array of strings, the same length as the number of independent
    variables. The strings are used to print a clean result, especially when
    we are dealing with the factor variables, where the dummy variable
    names can be very long due to the inserting of a random string to
    avoid naming conflicts, see \code{\link{as.factor,db.obj-method}}
    for details. The list also contains \code{dummy} and \code{dummy.expr}, which are also used for processing the categorical variables, but do not contain any important information.
  }

  \item{model}{
    A \code{\linkS4class{db.data.frame}} object, which wraps the model
    table of this function.
  }

  \item{model.summary}{
    A \code{\linkS4class{db.data.frame}} object, which wraps the summary
    table of this function.
  }

  \item{model.random}{
    A \code{\linkS4class{db.data.frame}} object, which wraps the kernel
    table of this function. Only created when non-linear kernel is used.
  }

  \item{terms}{ A \code{\link{terms}} object, describing the terms in
    the model formula.  }

  \item{nobs}{
    The number of observations used to fit the model.
  }

  \item{data}{
    A \code{db.obj} object, which wraps all the data used in the database.
    If there are fittings for multiple groups, then this is only the wrapper for the data in one group.
  }

  \item{origin.data}{
    The original \code{db.obj} object. When there is no grouping, it is equal to \code{data} above, otherwise it is the "sum" of \code{data} from all groups.
  }

  Note that if there is grouping done, and there are multiple
  \code{svm.madlib} objects in the final result, each one of them
  contains the same copy \code{model}.
}

\author{
  Author: Predictive Analytics Team at Pivotal Inc.

  Maintainer: Frank McQuillan, Pivotal Inc. \email{fmcquillan@pivotal.io}
}

\note{
  \code{|} is not part of standard R formula object, but many R packages
  use \code{|} to add their own functionalities into formula
  object. However, \code{|} has different meanings and usages
  in different packages. The user must be careful that usage of \code{|} in
  \code{\link{PivotalR-package}} may not be the same as the others.
}

\seealso{
  \code{\link{madlib.lm}},
  \code{\link{madlib.summary}}, \code{\link{madlib.arima}} are MADlib
  wrapper functions.
{}
  \code{\link{as.factor}} creates categorical variables for fitting.
{}
  \code{\link{delete}} safely deletes the result of this function.
}
\examples{
\dontrun{
%% @test .port Database port number
%% @test .dbname Database name
## set up the database connection
## Assume that .port is port number and .dbname is the database name
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)

data <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)
lk(data, 10)

## svm regression
## i.e. grouping on multiple columns
fit <- madlib.svm(length ~ height + shell | sex + (rings > 7), data = data, type = "regression")
fit

## use I(.) for expressions
fit <- madlib.svm(rings > 7 ~ height + shell + diameter + I(diameter^2),
                  data = data, type = "classification")
fit # display the result

## Adding new column for training
dat <- data
dat$arr <- db.array(data[,-c(1,2)])
array.data <- as.db.data.frame(dat)
fit <- madlib.svm(rings > 7 ~ arr, data = array.data)

db.disconnect(cid)
}
}

\keyword{madlib}
\keyword{stats}
