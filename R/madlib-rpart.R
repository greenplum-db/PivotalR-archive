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
    analyzer <- .get.params(formula, data, na.action, na.as.level)

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
                 gsub("(^array\\[|\\]$|\")", "", params1$ind.str), "', NULL, '",
                 params2$split, "', ", grp, ", ", weight.col, ", ",
                 params2$maxdepth, ", ", params2$minsplit, ", ", params2$minbucket, 
                 ", ", params2$nbins, ", 'cp=", params2$cp, "', ", verbose, ")", sep = "")
    res <- .db(sql, conn.id = conn.id, verbose = FALSE)

    model <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)
    model.summary <- db.data.frame(paste(tbl.output, "_summary", sep = ""), 
                                   conn.id = conn.id, verbose = FALSE)

    .restore.warnings(warnings)

    if (is.tbl.temp) delete(tbl.source, conn.id)

    rst <- list(model = model, model.summary = model.summary)
    class(rst) <- "dt.madlib"
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
    tbl.model <- content(x$model)
    conn.id <- conn.id(x$model)
    madlib <- schema.madlib(conn.id)
    sql <- paste("select ", madlib, ".tree_display('", 
                 gsub("\"", "", tbl.model), "', FALSE)", sep = "")
    cat(.db(sql, conn.id = conn.id, verbose = FALSE)[1,1], '\n')
}
