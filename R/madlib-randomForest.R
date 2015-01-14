## Wrapper function for MADlib's Random Forest

setClass("rf.madlib")

setClass("rf.madlib.grps")

madlib.randomForest <- function(formula, data, id = NULL,
                         ntree = 100, importance = FALSE, nPerm = 1,
                         na.action = NULL, control, na.as.level = FALSE,
                         verbose = FALSE, ...)
{
    ## Some validations
    if ( ! is( data, "db.obj" ) )
        stop( "madlib.rf can only be used on a db.obj object, ",
             "and ", deparse( substitute( data ) ), " is not!")

    if (missing(control)) control <- NULL
    ## Only newer versions of MADlib are supported for
    ## this function
    .check.madlib.version(data) #, allowed.version = 1.7)

    origin.data <- data # needed in the result report

    conn.id <- conn.id( data ) # database connection ID

    db <- .get.dbms.str(conn.id)
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
        stop("MADlib on HAWQ 1.1 does not support the latest random ",
             "forest module!")

    warnings <- .suppress.warnings(conn.id) # suppress SQL/R warnings

    ## analyze the formula
    #formula <- update(formula, ~ . - 1) # exclude constant
    f.str <- strsplit(paste(deparse(formula), collapse = ""), "\\|")[[1]]

    ## In order to deal with formula like " ~ . - id", we need a fake data
    ## frame with the same column names
    # fake.data <- as.data.frame(array(1, dim = c(1, length(names(data)))))
    # names(fake.data) <- names(data)
    #
    # f.str <- paste(c(paste(deparse(update(formula(f.str[1]), ~ . - 1)), collapse = ""),
    #                  if (is.na(f.str[2])) NULL else f.str[2]), collapse = " | ")

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
    params2 <- .extract.rf.params(control)

    if (is.null(id) && identical(key(data), character(0)))
        stop("MADlib random forest: you must specify an ID column!")
    else
        id.col <- if (is.null(id)) key(data) else id

    ## Construct SQL string
    tbl.source <- content(data) # data table name
    madlib <- schema.madlib(conn.id) # MADlib schema
    tbl.output <- .unique.string()
    sql <- paste("select ", madlib, ".forest_train('", 
                 tbl.source, 
                 "', '", 
                 tbl.output, 
                 "' , '", 
                 id.col, 
                 "', '", 
                 params1$dep.str, 
                 "', '", 
                 gsub("(^array\\[|\\]$)", "", params1$ind.str), 
                 "', NULL,", 
                 grp, 
                 ", ", 
                 ntree,
                 ",NULL",
                 ", ", 
                 importance, 
                 ", ", 
                 nPerm, 
                 ",", 
                 params2$maxdepth, 
                 ", ", 
                 params2$minsplit, 
                 ", ", 
                 params2$minbucket, 
                 ", ", 
                 params2$nbins, 
                 ", 'max_surrogates=", 
                 params2$max_surrogates, 
                 "', ", 
                 verbose, 
                 ")", 
                 sep = "")

    res <- .db(sql, conn.id = conn.id, verbose = FALSE)

    model <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)
    model.summary <- db.data.frame(paste(tbl.output, "_summary", sep = ""),
                                   conn.id = conn.id, verbose = FALSE)
    model.group <- db.data.frame(paste(tbl.output, "_group", sep = ""),
                                   conn.id = conn.id, verbose = FALSE)
    method <- if (lk(model.summary$is_classification)) "classification" else "regression"
    
    .restore.warnings(warnings)

    grouping.cols <- setdiff(names(model), c('tree', 'cat_levels_in_text', 'cat_n_levels', 'tree_depth'))
    if (length(grouping.cols) == 0)
        grouping.str <- ''
    else
        grouping.str <- paste(", ", paste(grouping.cols, collapse = ','))

    n_cats <- length(strsplit(lk(model.summary$cat_features), ",")[[1]])
    num_random_features <- lk(model.summary$num_random_features)
    ntrees <- lk(model.summary$num_trees)
    func_name <- paste(madlib, ".forest_train", sep = "")
    #create variable importance matrix
    cat_features  <- strsplit(lk(model.summary$cat_features),",")[[1]]
    con_features  <- strsplit(lk(model.summary$con_features),",")[[1]]
    cat_var_imp <- lk(model.group$cat_var_importance)
    con_var_imp  <- lk(model.group$con_var_importance)
    
    len_cat_features <- length(cat_features)
    len_con_features <- length(con_features)
    
    #populate result matrix for grouping vs non-grouping cases
    ngrps <- lk(model.summary$num_all_groups)
    if (ngrps == 1) { #no grouping
        cat_var_imp <- c(cat_var_imp[1,])
        con_var_imp <- c(con_var_imp[1,])

        len_cat_var_imp <- length(cat_var_imp)
        len_con_var_imp <- length(con_var_imp)
       
        cat_con_combined_var_imp <- c(cat_var_imp,con_var_imp)
        if (is.na(cat_var_imp[1])) {
            len_cat_var_imp <- 0
            cat_con_combined_var_imp <- con_var_imp
        }
        if (is.na(con_var_imp[1])) {
            len_con_var_imp <- 0
            cat_con_combined_var_imp <- cat_var_imp
        }
        if (is.na(cat_var_imp[1]) && is.na(con_var_imp[1])) {
            len_cat_var_imp <- 0
            len_con_var_imp <- 0
            cat_con_combined_var_imp <- NA
        }
     
        var_imp <- matrix(numeric(0), nrow=len_cat_features + len_con_features, ncol=1)
        rownames(var_imp)  <- c(cat_features, con_features)
        if (method == "classification") {
            colnames(var_imp) <- c('MeanDecreaseAccuracy')
        } else {
            colnames(var_imp) <- c('MeanIncreaseMSE')
        }
        if ((len_cat_var_imp + len_con_var_imp) == length(var_imp[,1])) {
            var_imp[,1] <- cat_con_combined_var_imp
        }
        rst <- list(model = model, model.summary = model.summary,
                    type = method, data = origin.data,
                    mtry = num_random_features, 
                    ntree = ntrees, call = func_name,
                    importance = var_imp)
        class(rst) <- "rf.madlib"
    } else { #grouping
        rst <- lapply(seq_len(ngrps), function(i) {
                    cat_var_imp <- c(cat_var_imp[i,])
                    con_var_imp <- c(con_var_imp[i,])
                    len_cat_var_imp <- length(cat_var_imp)
                    len_con_var_imp <- length(con_var_imp)
                   
                    cat_con_combined_var_imp <- c(cat_var_imp,con_var_imp)
                    if (is.na(cat_var_imp[1])) {
                        len_cat_var_imp <- 0
                        cat_con_combined_var_imp <- con_var_imp
                    }
                    if (is.na(con_var_imp[1])) {
                        len_con_var_imp <- 0
                        cat_con_combined_var_imp <- cat_var_imp
                    }
                    if (is.na(cat_var_imp[1]) && is.na(con_var_imp[1])) {
                        len_cat_var_imp <- 0
                        len_con_var_imp <- 0
                        cat_con_combined_var_imp <- NA
                    }
                 
                    var_imp <- matrix(numeric(0), nrow=len_cat_features + len_con_features, ncol=1)
                    rownames(var_imp)  <- c(cat_features, con_features)
                    if (method == "classification") {
                        colnames(var_imp) <- c('MeanDecreaseAccuracy')
                    } else {
                        colnames(var_imp) <- c('MeanIncreaseMSE')
                    }
                    if ((len_cat_var_imp + len_con_var_imp) == length(var_imp[,1])) {
                        var_imp[,1] <- cat_con_combined_var_imp
                    }
                     r <- list(model = model, model.summary = model.summary,
                               type = method, data = origin.data,
                               mtry = num_random_features, ntree = ntrees,
                               call = func_name,
                               importance = var_imp)
                    })
        for (i in seq_len(ngrps)) class(rst[[i]])  <- "rf.madlib"
        class(rst)  <- "rf.madlib.grps"
    }
    rst
}


## ------------------------------------------------------------

predict.rf.madlib <- function(object, newdata, type = c("response", "prob"), ...)
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
    sql = paste("select ", madlib, ".forest_predict('", .strip(content(object$model), "\""),
                "', '", sub("\".\"",".",.strip(content(newdata), "\"")), "', '", tbl.predict, "', '",
                type, "')", sep = "")
    .db(sql, conn.id = conn.id, verbose = FALSE)
    if (is.temp) delete(newdata)
    db.data.frame(tbl.predict, conn.id = conn.id, verbose = FALSE)
}

## ------------------------------------------------------------
## Function to retrieve a tree from the forest
## Result format is the same as R's randomForest 
## ------------------------------------------------------------

getTree.rf.madlib <- function (object, k=1, ...)
{
    tbl.output  <- attr(object$model, ".name")
    ntrees <- lk(object$model.summary$num_trees)
    if (k <= 0 || k > ntrees) {
        stop(paste("tree not found. maximum number of trees in forest is ",ntrees))
    }
    sql <- paste("select ", "madlib", "._convert_to_random_forest_format(tree ", " ) as frame ",
                 "from (select tree, row_number() OVER () as rnum from ", tbl.output, 
                 ")subq where subq.rnum = ", k, sep="")
    tree.info <- .db(sql)
    frame  <- tree.info$frame
    frame.matrix <- data.frame(matrix(arraydb.to.arrayr(frame,"numeric"),ncol=6))
    colnames(frame.matrix) <- c('left daughter','right daughter',
                         'split var','split point',
                         'status','prediction')
    frame.matrix
}
## ------------------------------------------------------------

## Extract other parameters
## control - a list, right now support "minsplit", "minbucket",
## "maxdepth" and "cp"
## Returns a list, which contains all the abaove. If some values
## are not given, default values are returned
.extract.rf.params <- function(control)
{
    default <- list( minsplit = 20, minbucket = round(20/3), maxdepth = 3,
                     max_surrogates = 0, nbins = 100)

    if ('minsplit' %in% names(control)) default$minsplit <- control$minsplit
    if ('minbucket' %in% names(control))
        default$minbucket <- control$minbucket
    else
        default$minbucket <- round(default$minsplit / 3)
    if ('maxdepth' %in% names(control)) default$maxdepth <- control$maxdepth
    if ('nbins' %in% names(control)) default$nbins <- control$nbins
    if ('max_surrogates' %in% names(control)) default$max_surrogates <- control$max_surrogates
    default
}

## ------------------------------------------------------------

print.rf.madlib <- function(x,
                            digits = max(3L, getOption("digits") - 3L),
                            ...)
{
    library(randomForest)
    class(x) <- "randomForest"
    out <- capture.output(print(x))
    writeLines(out)
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

## ------------------------------------------------------------

## borrow from rpart code

text.rf.madlib <- function(x, splits = TRUE, label, FUN = text, all = FALSE,
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

labels.rf.madlib <- function(object, digits = 4, minlength = 1L, pretty,
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
q

