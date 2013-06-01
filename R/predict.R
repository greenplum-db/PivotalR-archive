
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
    
    ## deal with groups
    coef.i <- which(names(object) == "coef")
    grp.col <- names(object)[seq_len(coef.i - 1)]

    if (length(object$grp.cols) == 0) {
        coef <- paste("array[", paste(object$coef, collapse = ", "), "]",
                      sep = "")
        expr <- paste(madlib, ".", func.str, "(", coef, ", ",
                      ind.str, ")", sep = "")
    } else {
        l <- length(object$grp.cols)
        expr <- "case when "
        for (i in seq_len(object$grps)) {
            tmp <- ""
            if (i != object$grps) {
                for (j in seq_len(l)) {
                    tmp <- paste(tmp, object$grp.cols[j], " = '",
                                 object[[object$grp.cols[j]]][i],
                                 "'::",
                                 newdata@.col.data_type[which(
                                     names(newdata) == object$grp.cols[j])],
                                 sep = "")
                    if (j != l) tmp <- paste(tmp, " and ", sep = "")
                }
                if (!is(newdata, "db.data.frame"))
                    tmp <- .replace.col.with.expr(tmp, names(newdata),
                                                  newdata@.expr)
                expr <- paste(expr, tmp, " then ", sep = "")
            }
            coef.i <- paste("array[", paste(object$coef[i,],
                                            collapse = ", "),
                            "]", sep = "")
            expr <- paste(expr, madlib, ".", func.str, "(", coef.i, ", ",
                          ind.str, ")", sep = "")
            if (i < object$grps - 1)
                expr <- paste(expr, " when ", sep = "")
            else if (i == object$grps - 1)
                expr <- paste(expr, " else ", sep = "")
            else
                expr <- paste(expr, " end", sep = "")
        }    
    }
    
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
