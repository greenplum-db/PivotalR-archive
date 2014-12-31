## Wrapper function for MADlib's decision tree

setClass("dt.madlib")

setClass("dt.madlib.grps")

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
    .check.madlib.version(data) #, allowed.version = 1.7)

    origin.data <- data # needed in the result report

    conn.id <- conn.id( data ) # database connection ID

    db <- .get.dbms.str(conn.id)
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
        stop("MADlib on HAWQ 1.1 does not support the latest decision ",
             "tree module!")

    warnings <- .suppress.warnings(conn.id) # suppress SQL/R warnings

    ## analyze the formula
    f.str <- strsplit(paste(deparse(formula), collapse = ""), "\\|")[[1]]

    f.str <- paste(c(paste(f.str[1], "- 1"),
                   if (is.na(f.str[2])) NULL else f.str[2]), collapse = " | ")
    formula <- formula(f.str)
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
                 ", ", params2$nbins, ", 'cp=", params2$cp, ", n_folds=", params2$n_folds,
                 "', 'max_surrogates=", params2$max_surrogates, "', ", verbose, ")", sep = "")
    res <- .db(sql, conn.id = conn.id, verbose = FALSE)

    model <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)
    model.summary <- db.data.frame(paste(tbl.output, "_summary", sep = ""),
                                   conn.id = conn.id, verbose = FALSE)
    method <- if (lk(model.summary$is_classification)) "class" else "anova"
    functions <- .assign.functions(method)

    .restore.warnings(warnings)

    grouping.cols <- setdiff(names(model), c('tree', 'cat_levels_in_text', 'cat_n_levels', 'tree_depth'))
    if (length(grouping.cols) == 0)
        grouping.str <- ''
    else
        grouping.str <- paste(", ", paste(grouping.cols, collapse = ','))

    n_cats <- length(strsplit(lk(model.summary$cat_features), ",")[[1]])
    tree.info <- .db("select ", madlib, "._convert_to_rpart_format(tree, ", n_cats, ") as frame, ",
                 "cat_levels_in_text, cat_n_levels, ", madlib,
                 "._get_split_thresholds(tree, ", n_cats, ") as thresholds", grouping.str,
                 " from ", sep = "", tbl.output, conn.id = conn.id, verbose = FALSE)
    cat_levels_in_text <- tree.info$cat_levels_in_text
    cat_n_levels <- tree.info$cat_n_levels
    thresholds <- tree.info$thresholds
    frame <- tree.info$frame

    n.grps <- nrow(tree.info) # how many groups
    frame.ncol <- .get.rpart.frame.ncol(model.summary)
    frame.matrix <- lapply(seq_len(n.grps), function(row)
                           data.frame(matrix(arraydb.to.arrayr(frame[row], "numeric"),
                                             ncol = frame.ncol)))
    frame.matrix <- .change.rpart.frame.colnames(frame.matrix, model.summary)
    frame.matrix <- .change.frame.rownames(frame.matrix)
    frame.matrix <- .replace.rpart.first.col(frame.matrix, model.summary)

    if (is.tbl.temp) delete(tbl.source, conn.id)

    splits <- .construct.splits(frame.matrix, model, model.summary, thresholds,
                                cat_levels_in_text, cat_n_levels)

    if (n.grps == 1) {
        rst <- list(model = model, model.summary = model.summary,
                    method=method, functions = functions, data = origin.data,
                    frame = frame.matrix[[1]], splits = splits$splits.list[[1]],
                    csplit = splits$csplit.list[[1]])
        attr(rst, "xlevels") <- splits$xlevels[[1]]
        class(rst) <- "dt.madlib"
        if (lk(model.summary$is_classification)) {
            attr(rst, 'ylevels') <- .strip(.strip(strsplit(lk(model.summary$dependent_var_levels), ",")[[1]]), "\"")
        }
    } else {
        rst <- lapply(seq_len(n.grps), function(i) {
                      r <- list(model = model, model.summary = model.summary,
                           method = method, functions = functions, data = origin.data,
                           frame = frame.matrix[[i]], splits = splits$splits.list[[i]],
                           csplit = splits$csplit.list[[i]], grp = tree.info[i, grouping.cols, drop=FALSE])
                      attr(r, "xlevels") <- splits$xlevels[[i]]
                      r
                    })
        for (i in seq_len(n.grps)) class(rst[[i]]) <- "dt.madlib"
        class(rst) <- "dt.madlib.grps"
        if (lk(model.summary$is_classification)) {
            for (i in 1:n.grps)
                attr(rst[[i]], "ylevels") <- .strip(.strip(strsplit(lk(model.summary$dependent_var_levels), ",")[[1]]), "\"")
        }
    }
    rst
}

## ------------------------------------------------------------

predict.dt.madlib <- function(object, newdata, type = c("response", "prob"), ...)
{
    type <- match.arg(type)
    if (missing(newdata)) newdata <- object$data
    if (is(newdata, "db.Rquery")) {
        newdata <- as.db.data.frame(newdata, verbose = FALSE)
        is.temp <- TRUE
    } else {
        is.temp <- FALSE
    }
    conn.id <- conn.id(newdata)
    tbl.predict <- .unique.string()
    madlib <- schema.madlib(conn.id)
    sql = paste("select ", madlib, ".tree_predict('", .strip(content(object$model), "\""),
                "', '", sub("\".\"",".",.strip(content(newdata), "\"")), "', '", tbl.predict, "', '",
                type, "')", sep = "")
    .db(sql, conn.id = conn.id, verbose = FALSE)
    if (is.temp) delete(newdata)
    db.data.frame(tbl.predict, conn.id = conn.id, verbose = FALSE)
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
                    cp = 0.01, nbins = 100, max_surrogates = 0,
                    n_folds = 0)

    if ('split' %in% names(parms)) default$split <- parms$split
    if ('minsplit' %in% names(control)) default$minsplit <- control$minsplit
    if ('minbucket' %in% names(control))
        default$minbucket <- control$minbucket
    else
        default$minbucket <- round(default$minsplit / 3)
    if ('maxdepth' %in% names(control)) default$maxdepth <- control$maxdepth
    if ('cp' %in% names(control)) default$cp <- control$cp
    if ('nbins' %in% names(control)) default$nbins <- control$nbins
    if ('max_surrogates' %in% names(control)) default$max_surrogates <- control$max_surrogates
    if ('n_folds' %in% names(control)) default$n_folds <- control$n_folds
    default
}

## ------------------------------------------------------------

print.dt.madlib <- function(x,
                            digits = max(3L, getOption("digits") - 3L),
                            ...)
{
    library(rpart)
    class(x) <- "rpart"
    out <- capture.output(print(x))
    if (!is.null(x$grp)) {
        cat(paste(names(x$grp), "=", as.vector(x$grp), collapse = ', '), '\n\n')
    }
    cat(paste(gsub(">=", ">", gsub("<", "<=", out)), collapse = "\n"), "\n")
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
                     #counts <- as.matrix(counts)
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
            frame[[i]]$yval2 <- as.matrix(frame[[i]]$yval2)
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
        frame[[i]][ , 1] <- sapply(frame[[i]][, 1], function(x) if (x < 0) "<leaf>" else features[x+1])
        frame[[i]]$ncompete <- 0
        frame[[i]]$nsurrogate[frame[[i]][,1] == "<leaf>"] <- 0
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

## ------------------------------------------------------------

string.bounding.box <- function(s)
{
    s2 <- strsplit(s, "\n")
    rows <- sapply(s2, length)
    columns <- sapply(s2, function(x) max(nchar(x, "w")))
    list(columns = columns, rows = rows)
}

## ------------------------------------------------------------

## Compute the correct row names of the frame
.change.frame.rownames <- function(frames)
{
    for (i in seq_len(length(frames))) {
        var <- frames[[i]]$var # the first column
        row <- rep(0, length(var))

        ## i - row index
        ## r - row name
        compute.rowname <- function(i, r) {
            row[i] <<- r
            if (i > length(var) || var[i] < 0) return (i+1)
            left <- compute.rowname(i+1, 2*r)
            right <- compute.rowname(left, 2*r+1)
        }

        compute.rowname(1, 1)
        row.names(frames[[i]]) <- as.character(row)
    }
    frames
}

## ------------------------------------------------------------
## The index of split node in splits matrix
.get.splits.index <- function(frame)
{
    ff <- frame
    n <- nrow(ff)
    is.leaf <- (ff$var == "<leaf>")
    whichrow <- !is.leaf
    vnames <- ff$var[whichrow]
    index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + (!is.leaf)))
    index
}

## ------------------------------------------------------------

## Construct the split matrix
.construct.splits <- function(frames, model, model.summary,
                              thresholds, cat.levels, cat.n)
{
    conn.id <- conn.id(model)
    madlib <- schema.madlib(conn.id)

    cat.features <- .strip(.strip(strsplit(lk(model.summary$cat_features), ",")[[1]], " "), "\"")

    splits.list <- list()
    csplit.list <- list()
    xlevels <- list()
    for (i in seq_len(length(frames))) {
        index <- .get.splits.index(frames[[i]])
        splits <- matrix(0, nrow=max(index)-1, ncol=4)
        splits[,2] <- 1
        is.leaf <- frames[[i]]$var == "<leaf>"
        splits[,4] <- matrix(arraydb.to.arrayr(thresholds[i], "double"), ncol=2)[,2]

        catn <- arraydb.to.arrayr(cat.n[i], "integer")
        cat.node <- frames[[i]]$var %in% cat.features
        if (sum(cat.node) > 0) {
            meaningful.cat <- index[-length(index)][!is.leaf & cat.node]
            splits[meaningful.cat, 2] <- catn[sapply(frames[[i]]$var[cat.node],
                                                      function(x) which(x == cat.features))]
            cat.thresh <- splits[meaningful.cat, 4] + 1
            splits[meaningful.cat, 4] <- seq_len(sum(cat.node))

            csplit <- matrix(2, nrow = sum(cat.node), ncol = max(splits[meaningful.cat, 2]))
            for (j in seq_len(nrow(csplit))) {
                csplit[j, 1:splits[meaningful.cat,2][j]] <- 1
                csplit[j, cat.thresh[j]] <- 3
            }

            csplit.list[[i]] <- csplit

            all.levels <- arraydb.to.arrayr(cat.levels[i], "character")
            levels <- list()
            count <- 0
            for (j in seq_along(cat.features)) {
                levels[[cat.features[j]]] <- all.levels[1:catn[j] + count]
                count <- count + catn[j]
            }
            xlevels[[i]] <- levels
        } else {
            csplit.list[[i]] <- NA
            xlevels[[i]] <- NA
        }

        splits.list[[i]] <- splits
    }
    list(splits.list=splits.list, csplit.list=csplit.list, xlevels=xlevels)
}

## ------------------------------------------------------------

## borrow from rpart code

text.dt.madlib <- function(x, splits = TRUE, label, FUN = text, all = FALSE,
                           pretty = NULL, digits = getOption("digits") - 3L,
                           use.n = FALSE, fancy = FALSE, fwidth = 0.8, fheight = 0.8,
                           bg = par("bg"), minlength = 1L, ...)
{
    #if (!inherits(x, "rpart")) stop("Not a legitimate \"rpart\" object")
    if (nrow(x$frame) <= 1L) stop("fit is not a tree, just a root")

    frame <- x$frame
    if (!missing(label)) warning("argument 'label' is no longer used")
    col <- names(frame)
    ylevels <- attr(x, "ylevels")
    if (!is.null(ylevels <- attr(x, "ylevels"))) col <- c(col, ylevels)
    cxy <- par("cxy")                   # character width and height
    if (!is.null(srt <- list(...)$srt) && srt == 90) cxy <- rev(cxy)
    xy <- eval(parse(text = "rpart:::rpartco(x)"))

    node <- as.numeric(row.names(frame))
    is.left <- (node %% 2L == 0L)            # left hand sons
    node.left <- node[is.left]
    parent <- match(node.left/2L, node)

    ## Put left splits at the parent node
    if (splits) {
        left.child <- match(2L * node, node)
        right.child <- match(node * 2L + 1L, node)
        rows <- if (!missing(pretty) && missing(minlength))
            labels(x, pretty = pretty) else labels(x, minlength = minlength)
        if (fancy) {
            ## put split labels on branches instead of nodes
            xytmp <- eval(parse(text = "rpart:::rpart.branch(x = xy$x, y = xy$y, node = node)"))
            leftptx <- (xytmp$x[2L, ] + xytmp$x[1L, ])/2
            leftpty <- (xytmp$y[2L, ] + xytmp$y[1L, ])/2
            rightptx <- (xytmp$x[3L, ] + xytmp$x[4L, ])/2
            rightpty <- (xytmp$y[3L, ] + xytmp$y[4L, ])/2

            FUN(leftptx, leftpty + 0.52 * cxy[2L],
                rows[left.child[!is.na(left.child)]], ...)
            FUN(rightptx, rightpty - 0.52 * cxy[2L],
                rows[right.child[!is.na(right.child)]], ...)
        } else
            FUN(xy$x, xy$y + 0.5 * cxy[2L], rows[left.child], ...)
    }

    leaves <- if (all) rep(TRUE, nrow(frame)) else frame$var == "<leaf>"

    stat <-
        x$functions$text(yval = if (is.null(frame$yval2)) frame$yval[leaves]
                         else frame$yval2[leaves, ],
                         dev = frame$dev[leaves], wt = frame$wt[leaves],
                         ylevel = ylevels, digits = digits,
                         n = frame$n[leaves], use.n = use.n)

    if (fancy) {
        if (col2rgb(bg, alpha = TRUE)[4L, 1L] < 255) bg <- "white"
        oval <- function(middlex, middley, a, b)
        {
            theta <- seq(0, 2 * pi, pi/30)
            newx <- middlex + a * cos(theta)
            newy <- middley + b * sin(theta)
            polygon(newx, newy, border = TRUE, col = bg)
        }

        ## FIXME: use rect()
        rectangle <- function(middlex, middley, a, b)
        {
            newx <- middlex + c(a, a, -a, -a)
            newy <- middley + c(b, -b, -b, b)
            polygon(newx, newy, border = TRUE, col = bg)
        }

        ## find maximum length of stat
        maxlen <- max(string.bounding.box(stat)$columns) + 1L
        maxht <- max(string.bounding.box(stat)$rows) + 1L

        a.length <- if (fwidth < 1)  fwidth * cxy[1L] * maxlen else fwidth * cxy[1L]

        b.length <- if (fheight < 1) fheight * cxy[2L] * maxht else fheight * cxy[2L]

        ## create ovals and rectangles here
        ## sqrt(2) creates the smallest oval that fits around the
        ## best fitting rectangle
        for (i in parent)
            oval(xy$x[i], xy$y[i], sqrt(2) * a.length/2, sqrt(2) * b.length/2)
        child <- match(node[frame$var == "<leaf>"], node)
        for (i in child)
            rectangle(xy$x[i], xy$y[i], a.length/2, b.length/2)
    }

    ##if FUN=text then adj=1 puts the split label to the left of the
    ##    split rather than centered
    ##Allow labels at all or just leaf nodes

    ## stick values on nodes
    if (fancy) FUN(xy$x[leaves], xy$y[leaves] + 0.5 * cxy[2L], stat, ...)
    else FUN(xy$x[leaves], xy$y[leaves] - 0.5 * cxy[2L], stat, adj = 0.5, ...)

    invisible()
}

labels.dt.madlib <- function(object, digits = 4, minlength = 1L, pretty,
                         collapse = TRUE, ...)
{
    if (missing(minlength) && !missing(pretty)) {
    minlength <- if (is.null(pretty)) 1L
    else if (is.logical(pretty)) {
        if (pretty) 4L else 0L
        } else 0L
    }

    ff <- object$frame
    n <- nrow(ff)
    if (n == 1L) return("root")            # special case of no splits

    is.leaf <- (ff$var == "<leaf>")
    whichrow <- !is.leaf
    vnames <- ff$var[whichrow] # the variable names for the primary splits

    index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + !is.leaf))
    irow <- index[c(whichrow, FALSE)] # we only care about the primary split
    ncat <- object$splits[irow, 2L]

    ## Now to work: first create labels for the left and right splits,
    ##  but not for leaves of course
    ##
    lsplit <- rsplit <- character(length(irow))

    if (any(ncat < 2L)) {               # any continuous vars ?
    jrow <- irow[ncat < 2L]
    cutpoint <- formatg(object$splits[jrow, 4L], digits)
    temp1 <- (ifelse(ncat < 0, "<=", "> "))[ncat < 2L]
    temp2 <- (ifelse(ncat < 0, "> ", "<="))[ncat < 2L]
    lsplit[ncat<2L] <- paste0(temp1, cutpoint)
    rsplit[ncat<2L] <- paste0(temp2, cutpoint)
    }

    if (any(ncat > 1L)) {               # any categorical variables ?
    xlevels <- attr(object, "xlevels")
    ##
    ## jrow will be the row numbers of factors within lsplit and rsplit
    ## crow the row number in "csplit"
    ## and cindex the index on the "xlevels" list
    ##
    jrow <- seq_along(ncat)[ncat > 1L]
    crow <- object$splits[irow[ncat > 1L], 4L] #row number in csplit
    cindex <- (match(vnames, names(xlevels)))[ncat > 1L]

    ## Now, abbreviate the levels
    if (minlength == 1L) {
        if (any(ncat > 52L))
        warning("more than 52 levels in a predicting factor, truncated for printout",
                        domain = NA)
        xlevels <- lapply(xlevels, function(z) c(letters, LETTERS)[pmin(seq_along(z), 52L)])
        } else if (minlength > 1L)
        xlevels <- lapply(xlevels, abbreviate, minlength, ...)

    ## Now tuck in the labels
    ## I'll let some other clever person vectorize this
    for (i in seq_along(jrow)) {
        j <- jrow[i]
        splits <- object$csplit[crow[i], ]
        ## splits will contain 1=left, 3=right, 2= neither
            cl <- if (minlength == 1L) "" else ","
            lsplit[j] <-
                paste((xlevels[[cindex[i]]])[splits == 1L], collapse = cl)
            rsplit[j] <-
                paste((xlevels[[cindex[i]]])[splits == 3L], collapse = cl)
        }
    }

    if (!collapse) {  # called by no routines that I know of
    ltemp <- rtemp <- rep("<leaf>", n)
    ltemp[whichrow] <- lsplit
    rtemp[whichrow] <- rsplit
    return(cbind(ltemp, rtemp))
    }

    lsplit <- paste0(ifelse(ncat < 2L, "", "="), lsplit)
    rsplit <- paste0(ifelse(ncat < 2L, "", "="), rsplit)

    ## Now match them up to node numbers
    ##   The output will have one label per row of object$frame, each
    ##   corresponding the the line segement joining this node to its parent
    varname <- (as.character(vnames))
    node <- as.numeric(row.names(ff))
    parent <- match(node %/% 2L, node[whichrow])
    odd <- (as.logical(node %% 2L))

    labels <- character(n)
    labels[odd] <- paste0(varname[parent[odd]], rsplit[parent[odd]])
    labels[!odd] <- paste0(varname[parent[!odd]], lsplit[parent[!odd]])
    labels[1L] <- "root"
    labels
}

