## Wrapper function for MADlib's decision tree

madlib.rpart <- function(formula, data, weights = NULL, id = NULL,
                         na.action = NULL, parms,
                         control, na.as.level = FALSE,
                         verbose = FALSE, ...)
{
    ## Some validations
    if ( ! is( data, "db.obj" ) )
        stop( "madlib.dt can only be used on a db.obj object, ",
             "and ", deparse( substitute( data ) ), " is not!")

    if (missing(parms)) parms <- NULL
    if (missing(control)) control <- NULL

    ## Only newer versions of MADlib are supported for
    ## this function
    .check.madlib.version( data )

    origin.data <- data # needed in the result report

    conn.id <- conn.id( data ) # database connection ID

    db <- .get.dbms.str(conn.id)
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
        stop("MADlib on HAWQ 1.1 does not support the latest decision ",
             "tree module!")

    warnings <- .suppress.warnings(conn.id) # suppress SQL/R warnings

    ## analyze the formula
    formula <- update(formula, ~ . - 1) # exclude constant
    analyzer <- .get.params(formula, data, na.action, na.as.level, FALSE)

    ## If data is db.view or db.Rquery, create a temporary table
    ## otherwise, use the original data
    data <- analyzer$data
    is.tbl.temp <- analyzer$is.tbl.source.temp
    ## dependent, independent and grouping variables
    ## and has.intercept flag
    params1 <- analyzer$params

    if (is.null(params1$grp.str))
    {
        grp <- "NULL"
    }
    else
    {
        grp <- paste("'", params1$grp.str, "'", sep = "")
    }


    ## Extract other parameters
    params2 <- .extract.dt.params(parms, control)

    weight.col <- if (is.null(weights)) "NULL" else paste("'", weights, "'", sep = "")
    if (is.null(id) && identical(key(data), character(0)))
        stop("MADlib decision tree: you must specify an ID column!")
    else
        id.col <- if (is.null(id)) key(data) else id

    ## Construct SQL string
    tbl.source <- content(data) # data table name
    madlib <- schema.madlib(conn.id) # MADlib schema
    tbl.output <- .unique.string()
    sql <- paste("select ", madlib, ".tree_train('", tbl.source,
                 "', '",  tbl.output, "', '", id.col, "', '",
                 params1$dep.str, "', '",
                 gsub("(^array\\[|\\]$)", "", params1$ind.str), "', NULL, '",
                 params2$split, "', ", grp, ", ", weight.col, ", ",
                 params2$maxdepth, ", ", params2$minsplit, ", ", params2$minbucket,
                 ", ", params2$nbins, ", 'cp=", params2$cp, "', ", verbose, ")", sep = "")
    res <- .db(sql, conn.id = conn.id, verbose = FALSE)

    model <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)
    model.summary <- db.data.frame(paste(tbl.output, "_summary", sep = ""),
                                   conn.id = conn.id, verbose = FALSE)
    method <- if (lk(model.summary$is_classification)) "class" else "anova"
    functions <- .assign.functions(method)

    .restore.warnings(warnings)

    frame <- .db("select ", madlib, "._convert_to_rpart_format(tree) as frame from ",
                 tbl.output, conn.id = conn.id, verbose = FALSE)
    n.grps <- nrow(frame) # how many groups
    frame.ncol <- .get.rpart.frame.ncol(model.summary)
    frame.matrix <- lapply(seq_len(n.grps), function(row)
                           data.frame(matrix(arraydb.to.arrayr(frame[row,], "numeric"),
                                             ncol = frame.ncol)))
    frame.matrix <- .change.rpart.frame.colnames(frame.matrix, model.summary)
    frame.matrix <- .replace.rpart.first.col(frame.matrix, model.summary)

    if (is.tbl.temp) delete(tbl.source, conn.id)

    if (n.grps == 1) {
        rst <- list(model = model, model.summary = model.summary,
                    method=method, functions = functions,
                    frame = frame.matrix[[1]])
        class(rst) <- "dt.madlib"
        if (lk(model.summary$is_classification)) {
            attr(rst, 'ylevels') <- .strip(.strip(strsplit(lk(model.summary$dependent_var_levels), ",")[[1]]), "\"")
        }
    } else {
        rst <- lapply(seq_len(n.grps), function(i)
                      list(model = model, model.summary = model.summary,
                           method = method, functions = functions,
                           frame = frame.matrix[[i]]))
        for (i in seq_len(n.grps)) class(rst[[i]]) <- "dt.madlib"
        class(rst) <- "dt.madlib.grp"
        if (lk(model.summary$is_classification)) {
            for (i in 1:n.grps)
                attr(rst[[i]], "ylevels") <- .strip(.strip(strsplit(lk(model.summary$dependent_var_levels), ",")[[1]]), "\"")
        }
    }
    rst
}

## ------------------------------------------------------------

## Extract other parameters
## parms - a list, right now only support "split"
## control - a list, right now support "minsplit", "minbucket",
## "maxdepth" and "cp"
## Returns a list, which contains all the abaove. If some values
## are not given, default values are returned
.extract.dt.params <- function(parms, control)
{
    default <- list(split = 'gini', minsplit = 20,
                    minbucket = round(20/3), maxdepth = 30,
                    cp = 0.01, nbins = 100)

    if ('split' %in% names(parms)) default$split <- parms$split
    if ('minsplit' %in% names(control)) default$minsplit <- control$minsplit
    if ('minbucket' %in% names(control))
        default$minbucket <- control$minbucket
    else
        default$minbucket <- round(default$minsplit / 3)
    if ('maxdepth' %in% names(control)) default$maxdepth <- control$maxdepth
    if ('cp' %in% names(control)) default$cp <- control$cp
    if ('nbins' %in% names(control)) default$nbins <- control$nbins
    default
}

## ------------------------------------------------------------

print.dt.madlib <- function(x,
                            digits = max(3L, getOption("digits") - 3L),
                            ...)
{
    library(rpart)
    class(x) <- "rpart"
    print(x)
}

## ------------------------------------------------------------

plot.dt.madlib <- function(x, uniform = FALSE, branch = 1, compress = FALSE, nspace,
                           margin = 0, minbranch = 0.3, ...)
{
    library(rpart)
    class(x) <- "rpart"
    plot(x, uniform=uniform, branch=branch, compress=compress, nspace=nspace,
         margin=margin, minbranch=minbranch, ...)
}

## ------------------------------------------------------------

text.dt.madlib <- function(x, splits = TRUE, label, FUN = text, all = FALSE,
                           pretty = NULL, digits = getOption("digits") - 3, use.n = FALSE,
                           fancy = FALSE, fwidth = 0.8, fheight = 0.8, bg = par("bg"),
                           minlength = 1L, ...)
{
    library(rpart)
    class(x) <- "rpart"
    if (missing(label)) label <- NULL
    text(x, splits=splits, label=label, FUN=FUN, all=all, pretty=pretty, digits=digits,
         use.n=use.n, fancy=fancy, fwidth=fwidth, fheight=fheight, bg=bg,
         minlength=minlength, ...)
}

## ------------------------------------------------------------

.assign.functions <- function(method)
{
    if (method=="anova") {
        list(summary = function (yval, dev, wt, ylevel, digits)
        {
            paste0("  mean=", formatg(yval, digits), ", MSE=", formatg(dev/wt,
                digits))
            },

            text = function (yval, dev, wt, ylevel, digits, n, use.n)
            {
                if (use.n)
                paste0(formatg(yval, digits), "\nn=", n)
                else formatg(yval, digits)
                })
        } else {
            list(
                 summary = function (yval, dev, wt, ylevel, digits)
                 {
                     nclass <- (ncol(yval) - 2L)/2L
                     group <- yval[, 1L]
                     counts <- yval[, 1L + (1L:nclass)]
                     yprob <- yval[, 1L + nclass + 1L:nclass]
                     nodeprob <- yval[, 2L * nclass + 2L]
                     if (!is.null(ylevel))
                         group <- ylevel[group]
                     temp1 <- formatg(counts, format = "%5g")
                     temp2 <- formatg(yprob, format = "%5.3f")
                     if (nclass > 1) {
                         temp1 <- apply(matrix(temp1, ncol = nclass), 1L, paste,
                                        collapse = " ")
                         temp2 <- apply(matrix(temp2, ncol = nclass), 1L, paste,
                                        collapse = " ")
                     }
                     dev <- dev/(wt[1L] * nodeprob)
                     paste0("  predicted class=", format(group, justify = "left"),
                            "  expected loss=", formatg(dev, digits), "  P(node) =",
                            formatg(nodeprob, digits), "\n", "    class counts: ",
                            temp1, "\n", "   probabilities: ", temp2)
                 },
                 print = function (yval, ylevel, digits)
                 {
                     temp <- if (is.null(ylevel))
                         as.character(yval[, 1L])
                     else ylevel[yval[, 1L]]
                     nclass <- (ncol(yval) - 2L)/2L
                     yprob <- if (nclass < 5L)
                         format(yval[, 1L + nclass + 1L:nclass], digits = digits,
                                nsmall = digits)
                     else formatg(yval[, 1L + nclass + 1L:nclass], digits = 2L)
                     if (!is.matrix(yprob))
                         yprob <- matrix(yprob, nrow = 1L)
                     temp <- paste0(temp, " (", yprob[, 1L])
                     for (i in 2L:ncol(yprob)) temp <- paste(temp, yprob[, i],
                                                             sep = " ")
                     temp <- paste0(temp, ")")
                     temp
                 },
                 text = function (yval, dev, wt, ylevel, digits, n, use.n)
                 {
                     nclass <- (ncol(yval) - 2L)/2L
                     group <- yval[, 1L]
                     counts <- yval[, 1L + (1L:nclass)]
                     if (!is.null(ylevel))
                         group <- ylevel[group]
                     temp1 <- formatg(counts, digits)
                     if (nclass > 1L)
                         temp1 <- apply(matrix(temp1, ncol = nclass), 1L, paste,
                                        collapse = "/")
                     if (use.n)
                         paste0(format(group, justify = "left"), "\n", temp1)
                     else format(group, justify = "left")
                 })
    }
}

## ------------------------------------------------------------

## get the dimension of the raprt frame matrix
.get.rpart.frame.ncol <- function(model.summary)
{
    if (lk(model.summary$is_classification))
        10 + 2 * length(strsplit(lk(model.summary$dependent_var_levels), ",")[[1]])
    else
        8
}

## ------------------------------------------------------------

## Change the column names of the frame data.frame
.change.rpart.frame.colnames <- function(frame, model.summary)
{
    col.names <- c('var', 'n', 'wt', 'dev', 'yval',
                   'complexity', 'ncompete', 'nsurrogate')
    if (lk(model.summary$is_classification)) {
        for (i in seq_len(length(frame))) {
            names(frame[[i]])[1:8] <- col.names
            ncl <- ncol(frame[[i]])
            frame[[i]]$yval2 <- frame[[i]][9]
            for (j in 10:ncl) {
                frame[[i]]$yval2 <- cbind(frame[[i]]$yval2, frame[[i]][j])
            }
            colnames(frame[[i]]$yval2) <- c(paste("V", 1:(ncl-9), sep = ""), "nodeprob")
            frame[[i]] <- frame[[i]][c(1:8, ncol(frame[[i]]))]
        }
    } else {
        for (i in seq_len(length(frame))) {
            names(frame[[i]]) <- col.names
        }
    }
    frame
}

## ------------------------------------------------------------

## Replace the first column values to be the split variable names
.replace.rpart.first.col <- function(frame, model.summary)
{
    features <- .strip(.strip(strsplit(lk(model.summary$independent_varnames), ",")[[1]]), "\"")
    for (i in seq_len(length(frame))) {
        for (j in 1:nrow(frame[[i]])) {
            frame[[i]][j, 1] <- if (frame[[i]][j,1] < 0) "<leaf>" else features[frame[[i]][j,1]]
        }
    }
    frame
}

## ------------------------------------------------------------

formatg <- function (x, digits = getOption("digits"),
                     format = paste0("%.",
                                     digits, "g"))
{
    if (!is.numeric(x))
        stop("'x' must be a numeric vector")
    temp <- sprintf(format, x)
    if (is.matrix(x))
        matrix(temp, nrow = nrow(x))
    else temp
}
