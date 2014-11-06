## Wrapper function for MADlib's random forest

setClass("randomForest.madlib")

madlib.randomForest <- function(formula, data, id = NULL, ntree = 100,
                                mtry = ceiling(.632*nrow(x)),
                                importance = FALSE, nPerm = 1,
                                na.action = NULL, na.as.level = FALSE,
                                control, verbose = FALSE, ...)
{
    ## argument validations
    if (!is(data, "db.obj"))
        stop("madlib.randomForest can only be used on a db.obj object, ",
             "and ", deparse(substitute(data)), " is not!")

    if (missing(control)) control <- NULL

    ## Only newer versions of MADlib are supported for
    ## this function
    .check.madlib.version(data) #, allowed.version = 1.7)

    origin.data <- data

    conn.id <- conn.id(data)

    db <- .get.dbms.str(conn.id)
    if (db$db.str == "HAWQ" && grepl("^1\\.1", db$version.str))
        stop("MADlib on HAWQ 1.1 does not support the latest decision ",
             "tree module!")

    warnings <- .suppress.warnings(conn.id)

    ## analyze the formula
    f.str <- strsplit(paste(deparse(formula), collapse = ""), "\\|")[[1]]
    f.str <- paste(c(paste(f.str[1], "- 1"),
                     if (is.na(f.str[2])) NULL else f.str[2]), collapse = " | ")
    formula <- formula(f.str)
    analyzer <- .get.params(formula, data, na.action, na.as.level, FALSE)

    ## If data is db.view or db.Rquery, create a temporary table first.
    ## Otherwise, use the original data
    data <- analyzer$data
    is.tbl.temp <- analyzer$is.tbl.source.temp
    params1 <- analyzer$params

    if  (is.null(params1$grp.str))
        grp <- "NULL"
    else
        grp <- paste("'", params1$grp.str, "'", sep = "")

    ## Extract other parameters that control the decision tree
    params2 <- .extract.dt.params.rf(control)

    if (is.null(id) && identical(key(data), character(0)))
        stop("MADlib random forest: you must specify an ID column!")
    else
        id.col <- if (is.null(id)) key(data) else id

    ## Construct SQL string
    tbl.source <- content(data)
    madlib <- schema.madlib(conn.id)
    tbl.output <- .unique.string()
    sql <- paste("select ", madlib, ".forest_train('", tbl.source,
                 "', '", tbl.output, "', '", id.col, "', '",
                 params1$dep.str, "', '",
                 gsub("(^array\\[|\\]$)", "", params1$ind.str), "', NULL,",
                 grp, ", ", ntree, ", ", mtry, ", ", params2$maxdepth, ", ",
                 params2$minsplit, ", ", params2$minbucket, ", ", params$nbins,
                 ", ", verbose, ", ", importance, ", ", nPerm, ")", sep = "")

    res <- .db(sql, conn.id = conn.id, verbose = FALSE)

    model <- db.data.frame(tbl.output, conn.id = conn.id, verbose = FALSE)
    model.summary <- db.data.frame(paste(tbl.output, "_summary", sep = ""),
                                   conn.id = conn.id, verbose = FALSE)
    model.group <- db.data.frame(paste(tbl.output, "_group", sep = ""),
                                 conn.id = conn.id, verbose = FALSE)

    .restore.warnings(warnings)
}
