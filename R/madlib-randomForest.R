## Wrapper function for MADlib's Random Forest

setClass("rf.madlib")

setClass("rf.madlib.grps")

madlib.randomForest <- function(formula, data, id = NULL,
                         ntree = 100, mtry = NULL, importance = FALSE,
                         nPerm = 1, na.action = NULL, control,
                         na.as.level = FALSE, verbose = FALSE, ...)
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

    if (is.null(mtry))
    {
        mtry <- "NULL"
    }
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
                 ", ",
                 mtry,
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
    if (requireNamespace("randomForest", quietly = TRUE)) {
        class(x) <- "randomForest"
        out <- capture.output(print(x))
        writeLines(out)
    } else {
        message("Error : Package randomForest needs to be installed for print")
        stop()
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

q

