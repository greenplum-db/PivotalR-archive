## Wrapper function for MADlib's decision tree

madlib.rpart <- function(formula, data, weights, subset,
                      na.action = na.rpart, method, parms,
                      control, na.as.level = FALSE, ...)
{
    ## Some validations
    if ( ! is( data, "db.obj" ) )
        stop( "madlib.dt can only be used on a db.obj object, ",
             "and ", deparse( substitute( data ) ), " is not!")

    ## Only newer versions of MADlib are supported for
    ## this function
    .check.madlib.data( data )

    origin.data <- data # needed in the result report

    conn.id <- conn.id( data ) # database connection ID

    db <- .get.dbms.str(conn.id)
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
        stop("MADlib on HAWQ 1.1 does not support the latest decision ",
             "tree module!")

    warnings <- .suppress.warnings(conn.id) # suppress SQL/R warnings

    ## analyze the formula
    formula <- update(formula, ~ . - 1) # exclude constant
    analyzer <- .get.param(formula, data, na.action, na.as.level)

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

    ## Construct SQL string
    tbl.source <- content(data) # data table name
    madlib <- schema.madlib(conn.id) # MADlib schema
    tbl.output <- .unique.string()
    sql <- paste()
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
                    cp = 0.01)

    if ('split' %in% names(parms)) default$split <- parms$split
    if ('minsplit' %in% names(control)) default$minsplit <- control$minsplit
    if ('minbucket' %in% names(control))
        default$minbucket <- control$minbucket
    else
        default$minbucket <- round(default$minsplit / 3)
    if ('maxdepth' %in% names(control)) default$maxdepth <- control$maxdepth
    if ('cp' %in% names(control)) default$cp <- control$cp
    default
}
