## Not to be called by the users, so all functions start with a prefix of dot
## and thus are not exported.

## Check arguments of function to make sure that they are legal

## ------------------------------------------------------------------------

## convert {...} string into an arrayparams$grp.str
arraydb.to.arrayr <- function (str, type = "double", n = 1)
{
    if (is.null(str)) return (NULL)
    
    if (type == "character")
        res <- character(0)
    else if (type == "integer")
        res <- integer(0)
    else if (type == "logical")
        res <- logical(0)
    else
        res <- numeric(0)
    
    for (i in seq(str))
    {
        if (is.na(str[i])) {
            res <- rbind(res, rep(NA, n))
            next
        }
        
        elm <- regmatches(str[i],
                          gregexpr("[^,\"\\s\\{\\}]+|\"[^\"]*\"",
                                   str[i], perl=T))[[1]]
        if (type == "character")
            elm <- as.character(elm)
        else if (type == "integer")
            elm <- as.integer(elm)
        else if (type == "logical")
            elm <- as.logical(elm)
        else
            elm <- as.numeric(elm)
        
        res <- rbind(res, elm)
    }
    row.names(res) <- NULL
    res
}

## ------------------------------------------------------------------------

.is.arg.string <- function (arg)
{
    return (!is.null(arg) &&
            is.character(arg))
}

## ------------------------------------------------------------------------

.is.conn.id.valid <- function (conn.id)
{
    return (length(conn.id) != 0 &&
            !is.null(conn.id) &&
            !is.na(conn.id) && 
            conn.id >= 1 &&
            length(.localVars$conn.id) != 0 &&
            conn.id %in% .localVars$conn.id[,1])
}

## ------------------------------------------------------------------------

## It is a little bit more complicated when schema name is not given.
## The table might be a temporary table, or a normal table. 
## .db.obj.info <- function (db.obj_name, conn.id = 1)
## {
##     parts <- strsplit(db.obj_name, "\\.")[[1]]
##     if (length(parts) == 2) {
##         table_schema <- parts[1]
##         table_name <- parts[2]
##     } else if (length(parts) == 1) {
##         table_name <- parts[1]
   
##         schemas <- arraydb.to.arrayr(.db.getQuery("select current_schemas(True)", conn.id),
##                                type = "character")
##         table_schema <- NULL
##         for (schema in schemas)
##         {
##             if (.db.existsTable(c(schema, table_name), conn.id)) {
##                 table_schema <- schema
##                 break
##             }
##         }
        
##         ## No such table, going to create a new one
##         ## usually in public
##         if (is.null(table_schema))
##             table_schema <- .db.getQuery("select current_schema()", conn.id)        
##     } else {
##         stop("The database object name is not valid!")
##     }
##     return (c(table_schema, table_name))
## }

## ------------------------------------------------------------------------

## get the distributed by string
.get.distributed.by.str <- function(conn.id, distributed.by)
{
    dbms.str <- dbms(conn.id)
    if (gsub(".*(Greenplum).*", "\\1", dbms.str, perl=T) == "Greenplum") {
        if (is.null(distributed.by))
            dist.str <- "DISTRIBUTED RANDOMLY"
        else {
            if (!.is.arg.string(distributed.by))
                stop("distributed.by must be a string or NULL!")
            if (distributed.by == "") # "" means no distributed by 
                dist.str <- ""
            else
                dist.str <- paste("DISTRIBUTED BY (", distributed.by, ")",
                                  sep = "")
        }
    } else
        dist.str <- ""
    return (dist.str)
}

## ------------------------------------------------------------------------

## simply return the most direct answer
.db.analyze.table.name <- function (name)
{
    parts <- strsplit(name, "\\.")[[1]]
    l <- length(parts)
    if (l != 1 && l != 2)
        stop("The database object name is not valid!")
    return (parts)
}

.db.table.schema.str <- function (table)
{
    l <- length(table)
    if (l == 2)
        return (paste("table_name = '", table[2], "' and table_schema = '",
                      table[1], "'", sep = ""))
    else if (l == 1)
        return (paste("table_name = '", table, "'", sep = ""))
}

## ------------------------------------------------------------------------

## Is the object in database a table?
.is.table.or.view <- function (table, conn.id = 1)
{
    pick <- .db.getQuery(
        paste("select count(*) from information_schema.tables where ",
              .db.table.schema.str(table), sep = ""), conn.id)
    if (pick == 1)
        return (TRUE)
    else
        return (FALSE)
}

## ------------------------------------------------------------------------

## Is the object in database a view?
.is.view <- function (table, conn.id = 1)
{
    pick <- .db.getQuery(
        paste("select count(*) from information_schema.views where ",
              .db.table.schema.str(table), sep = ""), conn.id)
    if (pick == 1)
        return (TRUE)
    else
        return (FALSE)
}

## ------------------------------------------------------------------------

.unique.string <- function ()
{
    r1 <- sample(seq_len(100000000), 1)
    r2 <- unclass(as.POSIXct(strptime(date(),"%c")))[1]
    r3 <- r2 %% r1
    paste("__madlib_temp_", r1, "_", r2, "_", r3, "__", sep = "")
}

## -----------------------------------------------------------------------

## strip the leading and trailing white spaces
.strip <- function (str, rm = "\\s")
{
    rm.str <- paste("^", rm, "*(.*[^", rm, "])", rm, "*$", sep = "")
    gsub(rm.str, "\\1", str, perl = TRUE)
}

## ------------------------------------------------------------------------

## analyze formula
.analyze.formula <- function (formula, data, fdata = data, refresh = FALSE,
                              is.factor = NA, cols = NA, suffix = NA)
{
    f.str <- strsplit(paste(deparse(formula), collapse = ""), "\\|")[[1]]
    fstr <- f.str[1]
    f1 <- formula(fstr) # formula
    f2 <- f.str[2] # grouping columns, might be NA

    if (!is.na(f2)) {
        f2.terms <- terms(formula(paste("~", f2)))
        f2.labels <- attr(f2.terms, "term.labels")
        inter <- intersect(f2.labels, names(fdata))
        if (length(inter) != length(f2.labels))
            stop("The grouping part of the formula is not quite right!")
        ## grouping column do not use factor
        f2.labels <- gsub("factor\\((.*)\\)", "\\1", f2.labels, perl = T)
        f2.labels <- gsub("factor\\((.*)\\)", "\\1", f2.labels, perl = T)
        f2.labels <- .replace.with.quotes(f2.labels, data@.col.name)
        grp <- paste(f2.labels, collapse = ", ")
    } else {
        f2.labels <- NULL
        grp <- NULL
    }

    ## create a fake data.frame only to extract
    ## terms when there is "." in formula
    fake.data <- data.frame(t(names(fdata)))
    colnames(fake.data) <- names(fdata)
    f.terms <- terms(f1, data = fake.data) # formula terms
    ## the 1st row is the dependent variable
    f.factors <- attr(f.terms, "factors")
    f.labels <- attr(f.terms, "term.labels") # each terms on the right side

    right.hand <- paste(f.labels, collapse = "+")
    if (refresh) {
        replace.cols <- cols[is.factor]
        suffix <- suffix[is.factor]
        n.order <- order(nchar(replace.cols), decreasing = TRUE)
        replace.cols <- replace.cols[n.order]
        suffix <- suffix[n.order]
        for (i in seq_len(length(replace.cols))) {
            col <- replace.cols[i]
            new.col <- names(data)[grep(paste(col, suffix[i], sep=""),
                                        names(data))]
            new.col <- paste("(", paste(new.col, collapse = " + "), ")",
                             sep = "")
            right.hand <- gsub(paste(col, "([^_\\w]+|$)", sep = ""),
                               paste(new.col, "\\1", sep = ""),
                               right.hand, perl = TRUE)
        }
    } else {
        ## find all the factor columns
        right.hand <- gsub("as.factor\\s*\\((.*)\\)",
                           "factor(\\1)", right.hand, perl = T)
        elm <- regmatches(right.hand,
                          gregexpr("factor\\s*\\([^\\(\\)]+\\)",
                                   right.hand, perl=T))[[1]]
        col <- .strip(gsub("factor\\s*\\(([^\\(\\)]+)\\)", "\\1", elm,
                           perl = T))
        if (!all(col %in% names(data)))
            stop("You can only make a existing column of",
                 " the data into factor!")
        for (cl in col) data[[cl]] <- as.factor(data[[cl]])
    }

    right.hand <- gsub("as.factor\\((.*)\\)", "\\1", right.hand, perl = T)
    right.hand <- gsub("factor\\((.*)\\)", "\\1", right.hand, perl = T)

    f.terms1 <- terms(formula(paste("~", right.hand)), data = fake.data)
    f.labels <- attr(f.terms1, "term.labels")

    f.intercept <- attr(f.terms, "intercept")
    labels <- gsub(":", "*", f.labels, perl = T) # replace interaction : with *
    labels <- gsub("I\\((.*)\\)", "\\1", labels, perl = T) # remove I()

    ## dependent variable
    ## factor does not play a role in dependent variable
    dep.var <- gsub("I\\((.*)\\)", "\\1", rownames(f.factors)[1], perl = T)
    dep.var <- gsub("as.factor\\((.*)\\)", "\\1", dep.var, perl = T)
    dep.var <- gsub("factor\\((.*)\\)", "\\1", dep.var, perl = T)
    dep.var <- .replace.with.quotes(dep.var, data@.col.name)

    ## with or without intercept
    if (f.intercept == 0)
        intercept.str <- ""
    else
        intercept.str <- "1,"

    labels <- .is.array(labels, data)

    labels <- .replace.with.quotes(labels, data@.col.name)
    ## remove grouping columns, when there is no intercept term
    if (!is.null(f2.labels) && f.intercept != 0)
        labels <- setdiff(labels, f2.labels)
    ind.var <- paste("array[", intercept.str,
                     paste(labels, collapse = ","),
                     "]", sep = "") # independent variable

    list(dep.str = dep.var, ind.str = ind.var, grp.str = grp,
         ind.vars = labels,
         has.intercept = as.logical(f.intercept),
         data = data)
}

## ------------------------------------------------------------------------

.replace.with.quotes <- function (vars, cols)
{
    n.order <- order(nchar(cols), decreasing = TRUE)
    cols <- cols[n.order]
    for (i in seq_len(length(cols))) {
        col <- cols[i]
        vars <- gsub(paste(col, "([^_\\w]+)", sep = ""),
                     paste("\"", col, "\"\\1", sep = ""),
                     vars, perl = TRUE)
        vars <- gsub(paste("([_\\w]+)\"", col, "\"", sep = ""),
                     paste("\\1", col, sep = ""),
                     vars, perl = TRUE)
        vars <- gsub(paste("([^_\\w\"]+)", col, sep = ""),
                     paste("\\1\"", col, "\"", sep = ""),
                     vars, perl = TRUE)
        vars <- gsub(paste("\"", col, "\"([_\\w]+)", sep = ""),
                     paste(col, "\\1", sep = ""),
                     vars, perl = TRUE)
        vars <- gsub(paste("^", col, "$", sep = ""),
                     paste("\"", col, "\"", sep = ""),
                     vars, perl = TRUE)
    }
    ## vars <- gsub("([\\w_]+)", "\"\\1\"", vars, perl = T)
    vars
}

## ------------------------------------------------------------------------

## interactively read an input from the user
.read.input <- function (msg, expected.answer = c("yes", "y", "no", "n"))
{
    a <- tolower(readline(msg))
    while (! a %in% expected.answer)
        a <- tolower(readline(msg))
    a
}

## ------------------------------------------------------------------------

## set/reset the message level
## returns the old message level
.set.msg.level <- function (level, conn.id)
{
    old.level <- .db.getQuery("select setting from pg_settings where name='client_min_messages'", conn.id)
    .db.getQuery(paste("set client_min_messages to", level), conn.id)
    old.level
}

## ------------------------------------------------------------------------

## If an independent variable is an array, it needs special treatment
.is.array <- function (labels, data)
{
    nlabels <- character(0)
    for (i in seq_len(length(labels))) {
        if (labels[i] %in% names(data) && data@.col.data_type == "ARRAY") {
            n <- .db.getQuery(paste("select array_upper(", labels[i], ",1) from ",
                              content(data), " limit 1", sep = ""), conn.id(data))[[1]]
            nlabels <- c(nlabels, paste(labels[i], "[", seq_len(n), "]", sep = ""))
        } else {
            nlabels <- c(nlabels, labels[i])
        }
    }
    nlabels
}

## ------------------------------------------------------------------------

## whether two where strings are equivalent
.eql.where <- function (where1, where2)
{
    s <- gsub("^not \\((.*)\\)$", "\\1", where1, perl = TRUE)
    t <- gsub("^not \\((.*)\\)$", "\\1", where2, perl = TRUE)
    if (s != where1 && t != where2) {
        return (.eql.where(s, t))
    } else if ((s != where1 && t == where2) ||
               (s == where1 && t != where2)) {
        return (FALSE)
    }
    
    s1 <- gsub("^\\((.*)\\) (and|or) \\((.*)\\)$", "\\1", s, perl = TRUE)
    if (s1 == s) return (s == t)
    s2 <- gsub("^\\((.*)\\) (and|or) \\((.*)\\)$", "\\3", s, perl = TRUE)
    ss <- gsub("^\\((.*)\\) (and|or) \\((.*)\\)$", "\\2", s, perl = TRUE)

    t1 <- gsub("^\\((.*)\\) (and|or) \\((.*)\\)$", "\\1", t, perl = TRUE)
    if (t1 == t) return (FALSE)
    t2 <- gsub("^\\((.*)\\) (and|or) \\((.*)\\)$", "\\3", t, perl = TRUE)
    tt <- gsub("^\\((.*)\\) (and|or) \\((.*)\\)$", "\\2", t, perl = TRUE)

    if ((.eql.where(s1, t1) && .eql.where(s2, t2) && ss == tt) ||
        (.eql.where(s1, t2) && .eql.where(s2, t1) && ss == tt))
        TRUE
    else
        FALSE
}
