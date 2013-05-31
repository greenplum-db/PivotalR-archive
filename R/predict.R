
## ------------------------------------------------------------------------
## Predict
## ------------------------------------------------------------------------

predict.lm.madlib <- function (object, newdata, ...)
{
    .predict(object, newdata, "linregr_predict")
}

## ------------------------------------------------------------------------

predict.logregr.madlib <- function (object, newdata, ...)
{
    .predict(object, newdata, "logregr_predict")
}

## ------------------------------------------------------------------------

.predict <- function (object, newdata, func.str)
{
    if (!is(newdata, "db.obj"))
        stop("New data for prediction must be a db.obj!")

    ## deal with groups
    coef.i <- which(names(object) == "coef")
    grp.col <- names(object)[seq_len(coef.i - 1)]

    if (length(grp.cols) == 0) {
        coef <- paste("array[", paste(object$coef, collapse = ", "), "]",
                      sep = "")
    } else {
        terms <- .strip(strsplit(newdata@.where, "and")[[1]])
        cond.str <- character(0)
        for (i in seq_len(length(terms))) {
            a <- terms[i]
            b <- gsub("")
        }
    }
    
    madlib <- schema.madlib(conn.id(newdata))
    if (is(newdata, "db.data.frame")) {
        tbl <- content(newdata)
        src <- tbl
        parent <- src
        where <- ""
        where.str <- ""
        sort <- list(by = "", order = "", str = "")
    } else {
        if (newdata@.source == newdata@.parent)
            tbl <- newdata@.parent
        else
            tbl <- paste("(", newdata@.parent, ") s", sep = "")
        src <- newdata@.source
        parent <- newdata@.parent
        where <- newdata@.where
        if (where != "") where.str <- paste(" where", where)
        else where.str <- ""
        sort <- newdata@.sort
    }
    if (!is(newdata, "db.data.frame"))
        ind.str <- .replace.col.with.expr(object$ind.str, names(newdata),
                                          newdata@.expr)
    else
        ind.str <- object$ind.str
    expr <- paste(madlib, ".", func.str, "(", coef, ", ",
                  ind.str, ")", sep = "")
    sql <- paste("select ", expr, " as madlib_predict from ",
                 tbl, where.str, sort$str, sep = "")

    new("db.Rquery",
        .content = sql,
        .expr = expr,
        .source = src,
        .parent = parent,
        .conn.id = conn.id(newdata),
        .col.name = "madlib_predict",
        .key = character(0),
        .col.data_type = "double precision",
        .col.udt_name = "float8",
        .where = where,
        .is.factor = FALSE,
        .sort = sort)
}

## ------------------------------------------------------------------------

.replace.col.with.expr <- function (str, cols, expr)
{
    for (i in seq_len(length(expr))) {
        ep <- expr[i]
        col <- cols[i]
        str <- gsub(paste("([\\+\\-\\*/\\s%]|^)", col,
                          "([\\+\\-\\*/\\s%]|$)", sep = ""),
                    paste("\\1", ep, "\\2", sep = ""),
                    str, perl = TRUE)
    }
    str
}
