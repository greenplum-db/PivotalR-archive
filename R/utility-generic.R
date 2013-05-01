## Not to be called by the users, so all functions start with a prefix of dot
## and thus are not exported.

## Check arguments of function to make sure that they are legal

## ------------------------------------------------------------------------

## convert {...} string into an arrayparams$grp.str
arraydb.to.arrayr <- function (str, type = "double")
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
        elm <- regmatches(str[i],
                          gregexpr("[^,\"\\s\\{\\}]+|\"(\\\"|[^\"])*\"",
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

## ------------------------------------------------------------------------

## analyze formula
.analyze.formula <- function (formula, data)
{
    f.str <- strsplit(deparse(formula), "\\|")[[1]]
    f1 <- formula(f.str[1]) # formula
    f2 <- f.str[2] # grouping columns, might be NA
    if (!is.na(f2)) {
        f2.terms <- terms(formula(paste("~", f2)))
        f2.labels <- attr(f2.terms, "term.labels")
        inter <- intersect(f2.labels, names(data))
        if (length(inter) != length(f2.labels))
            stop("The grouping part of the formula is not quite right!")
        grp <- paste(f2.labels, collapse = ", ")
    } else {
        f2.labels <- NULL
        grp <- NULL
    }
    
    ## create a fake data.frame only to extract
    ## terms when there is "." in formula
    fake.data <- data.frame(t(names(data)))
    colnames(fake.data) <- names(data)
    f.terms <- terms(f1, data = fake.data) # formula terms
    ## the 1st row is the dependent variable
    f.factors <- attr(f.terms, "factors")
    f.labels <- attr(f.terms, "term.labels") # each terms on the right side
    f.intercept <- attr(f.terms, "intercept")
    labels <- gsub(":", "*", f.labels) # replace interaction : with *
    labels <- gsub("I\\((.*)\\)", "\\1", labels) # remove I()
    ## remove grouping columns, when there is no intercept term
    if (!is.null(f2.labels) && f.intercept != 0) 
        labels <- setdiff(labels, f2.labels)
    ##
    ## dependent variable
    dep.var <- gsub("I\\((.*)\\)", "\\1", rownames(f.factors)[1]) 
    ## with or without intercept
    if (f.intercept == 0)
        intercept.str <- ""
    else
        intercept.str <- "1,"
    ind.var <- paste("array[", intercept.str,
                     paste(labels, collapse = ","),
                     "]", sep = "") # independent variable
    ##
    list(dep.str = dep.var, ind.str = ind.var, grp.str = grp,
         ind.vars = labels,
         has.intercept = as.logical(f.intercept))
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
.set.msg.level <- function (level)
{
    old.level <- .db.getQuery("select setting from pg_settings where name='client_min_messages'")
    .db.getQuery(paste("set client_min_messages to", level))
    old.level
}
