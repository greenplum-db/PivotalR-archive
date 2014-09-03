\name{madlib.rpart}
\alias{madlib.rpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
madlib.rpart(formula, data, weights = NULL, id = NULL, na.action = NULL, parms, control, na.as.level = FALSE, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{na.action}{
%%     ~~Describe \code{na.action} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
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

## The function is currently defined as
function (formula, data, weights = NULL, id = NULL, na.action = NULL, 
    parms, control, na.as.level = FALSE, verbose = FALSE, ...) 
{
    if (!is(data, "db.obj")) 
        stop("madlib.dt can only be used on a db.obj object, ", 
            "and ", deparse(substitute(data)), " is not!")
    if (missing(parms)) 
        parms <- NULL
    if (missing(control)) 
        control <- NULL
    .check.madlib.version(data)
    origin.data <- data
    conn.id <- conn.id(data)
    db <- .get.dbms.str(conn.id)
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str)) 
        stop("MADlib on HAWQ 1.1 does not support the latest decision ", 
            "tree module!")
    warnings <- .suppress.warnings(conn.id)
    f.str <- strsplit(paste(deparse(formula), collapse = ""), 
        "\\|")[[1]]
    f.str <- paste(c(paste(deparse(update(formula(f.str[1]), 
        ~. - 1)), collapse = ""), if (is.na(f.str[2])) NULL else f.str[2]), 
        collapse = " | ")
    formula <- formula(f.str)
    analyzer <- .get.params(formula, data, na.action, na.as.level, 
        FALSE)
    data <- analyzer$data
    is.tbl.temp <- analyzer$is.tbl.source.temp
    params1 <- analyzer$params
    if (is.null(params1$grp.str)) {
        grp <- "NULL"
    }
    else {
        grp <- paste("'", params1$grp.str, "'", sep = "")
    }
    params2 <- .extract.dt.params(parms, control)
    weight.col <- if (is.null(weights)) 
        "NULL"
    else paste("'", weights, "'", sep = "")
    if (is.null(id) && identical(key(data), character(0))) 
        stop("MADlib decision tree: you must specify an ID column!")
    else id.col <- if (is.null(id)) 
        key(data)
    else id
    tbl.source <- content(data)
    madlib <- schema.madlib(conn.id)
    tbl.output <- .unique.string()
    sql <- paste("select ", madlib, ".tree_train('", tbl.source, 
        "', '", tbl.output, "', '", id.col, "', '", params1$dep.str, 
        "', '", gsub("(^array\\[|\\]$)", "", params1$ind.str), 
        "', NULL, '", params2$split, "', ", grp, ", ", weight.col, 
        ", ", params2$maxdepth, ", ", params2$minsplit, ", ", 
        params2$minbucket, ", ", params2$nbins, ", 'cp=", params2$cp, 
        "', ", verbose, ")", sep = "")
    res <- .db(sql, conn.id = conn.id, verbose = FALSE)
    model <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)
    model.summary <- db.data.frame(paste(tbl.output, "_summary", 
        sep = ""), conn.id = conn.id, verbose = FALSE)
    method <- if (lk(model.summary$is_classification)) 
        "class"
    else "anova"
    functions <- .assign.functions(method)
    .restore.warnings(warnings)
    n_cats <- length(strsplit(lk(model.summary$cat_features), 
        ",")[[1]])
    tree.info <- .db("select ", madlib, "._convert_to_rpart_format(tree, ", 
        n_cats, ") as frame, ", "cat_levels_in_text, cat_n_levels, ", 
        madlib, "._get_split_thresholds(tree) as thresholds from ", 
        tbl.output, conn.id = conn.id, verbose = FALSE)
    cat_levels_in_text <- tree.info$cat_levels_in_text
    cat_n_levels <- tree.info$cat_n_levels
    thresholds <- tree.info$thresholds
    frame <- tree.info$frame
    n.grps <- nrow(tree.info)
    frame.ncol <- .get.rpart.frame.ncol(model.summary)
    frame.matrix <- lapply(seq_len(n.grps), function(row) data.frame(matrix(arraydb.to.arrayr(frame[row], 
        "numeric"), ncol = frame.ncol)))
    frame.matrix <- .change.rpart.frame.colnames(frame.matrix, 
        model.summary)
    frame.matrix <- .change.frame.rownames(frame.matrix)
    frame.matrix <- .replace.rpart.first.col(frame.matrix, model.summary)
    if (is.tbl.temp) 
        delete(tbl.source, conn.id)
    splits <- .construct.splits(frame.matrix, model, model.summary, 
        thresholds, cat_levels_in_text, cat_n_levels)
    if (n.grps == 1) {
        rst <- list(model = model, model.summary = model.summary, 
            method = method, functions = functions, data = origin.data, 
            frame = frame.matrix[[1]], splits = splits$splits.list[[1]], 
            csplit = splits$csplit.list[[1]])
        attr(rst, "xlevels") <- splits$xlevels[[1]]
        class(rst) <- "dt.madlib"
        if (lk(model.summary$is_classification)) {
            attr(rst, "ylevels") <- .strip(.strip(strsplit(lk(model.summary$dependent_var_levels), 
                ",")[[1]]), "\"")
        }
    }
    else {
        rst <- lapply(seq_len(n.grps), function(i) {
            r <- list(model = model, model.summary = model.summary, 
                method = method, functions = functions, data = origin.data, 
                frame = frame.matrix[[i]], splits = splits$splits.list[[i]], 
                csplit = splits$csplit.list[[i]])
            attr(r, "xlevels") <- splits$xlevels[[i]]
        })
        for (i in seq_len(n.grps)) class(rst[[i]]) <- "dt.madlib"
        class(rst) <- "dt.madlib.grp"
        if (lk(model.summary$is_classification)) {
            for (i in 1:n.grps) attr(rst[[i]], "ylevels") <- .strip(.strip(strsplit(lk(model.summary$dependent_var_levels), 
                ",")[[1]]), "\"")
        }
    }
    rst
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
