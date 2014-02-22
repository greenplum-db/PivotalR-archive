## -----------------------------------------------------------------------
## utility functions exposed to the users
## -----------------------------------------------------------------------

is.db.data.frame <- function (x)
{
    is(x, "db.data.frame")
}

## -----------------------------------------------------------------------

## cut the data into k pieces
## mainly used for cross-validation
.cut.data <- function (x, k)
{
    ## randomize the data when creating serial index
    y <- .create.indexed.temp.table(x, TRUE)
    n <- dim(y)[1]

    if (n < k) stop("data dimension is even smaller than k!")

    id <- y@.dim[2]
    size <- n %/% k
    tick <- c(0, seq(size, length.out = k-1, by = size), n)
    valid <- list()
    train <- list()
    for (i in 1:k) {
        valid[[i]] <- y[y[,id]>tick[i] & y[,id]<=tick[i+1],-id]
        train[[i]] <- y[!(y[,id]>tick[i] & y[,id]<=tick[i+1]),-id]
    }

    list(train = train, valid = valid, inter = y, dist.by = y@.dist.by)
}

## ----------------------------------------------------------------------

## suppress all warnings
.suppress.warnings <- function (conn.id, level = "panic")
{
    msg.level <- .set.msg.level(level, conn.id = conn.id)
    warn.r <- getOption("warn")
    options(warn = -1)
    list(msg.level = msg.level, warn.r = warn.r, conn.id = conn.id)
}

## ----------------------------------------------------------------------

## restore all warning levels
.restore.warnings <- function (pre.warn)
{
    msg.level <- .set.msg.level(pre.warn$msg.level, pre.warn$conn.id)
    options(warn = pre.warn$warn.r) # reset R warning level
}

## ----------------------------------------------------------------------

## Compatibility function
## Find the package path, for older R path.package does not work
.get.package.path <- function ()
{
    version <- .localVars$R.ver
    if (version != R.version.string && as.numeric(version) < 2.13)
        eval(parse(text = paste(".path.package(\"", .this.pkg.name, "\")",
                   sep = "")))
    else
        path.package(.this.pkg.name)
}

## ----------------------------------------------------------------------

## Compatibility function
# regmatches for lower versions of R
.regmatches <- function (x, m, invert = FALSE)
{
    version <- .localVars$R.ver
    if (version != R.version.string && as.numeric(version) < 2.14) {
        if (length(x) != length(m))
            stop(gettextf("%s and %s must have the same length",
                          sQuote("x"), sQuote("m")), domain = NA)
        ili <- is.list(m)
        useBytes <- if (ili)
            any(unlist(lapply(m, attr, "useBytes")))
        else any(attr(m, "useBytes"))
        if (useBytes) {
            asc <- iconv(x, "latin1", "ASCII")
            ind <- is.na(asc) | (asc != x)
            if (any(ind))
                Encoding(x[ind]) <- "bytes"
        }
        if (!ili && !invert) {
            so <- m[ind <- (!is.na(m) & (m > -1L))]
            eo <- so + attr(m, "match.length")[ind] - 1L
            return(substring(x[ind], so, eo))
        }
        y <- if (invert) {
            Map(function(u, so, ml) {
                if ((n <- length(so)) == 1L) {
                    if (is.na(so))
                        return(character())
                    else if (so == -1L)
                        return(u)
                }
                beg <- if (n > 1L) {
                    eo <- so + ml - 1L
                    if (any(eo[-n] >= so[-1L]))
                        stop(gettextf("need non-overlapping matches for %s",
                                      sQuote("invert = TRUE")), domain = NA)
                    c(1L, eo + 1L)
                }
                else {
                    c(1L, so + ml)
                }
                end <- c(so - 1L, nchar(u))
                substring(u, beg, end)
            }, x, m, if (ili)
                lapply(m, attr, "match.length")
            else attr(m, "match.length"), USE.NAMES = FALSE)
        }
        else {
            Map(function(u, so, ml) {
                if (length(so) == 1L) {
                    if (is.na(so) || (so == -1L))
                        return(character())
                }
                substring(u, so, so + ml - 1L)
            }, x, m, lapply(m, attr, "match.length"), USE.NAMES = FALSE)
        }
        names(y) <- names(x)
        y
    } else {
        regmatches(x, m, invert)
    }
}

## ----------------------------------------------------------------------

.format <- function(str, lst)
{
    for (item in names(lst))
        str <- gsub(paste("<", item, ">", sep = ""), lst[[item]], str)
    gsub("\n", "", str)
}

## ----------------------------------------------------------------------

## get the distributed by info
.get.dist.policy <- function (table, conn.id)
{
    ## get the table name and schema
    ## conn.id <- conn.id(x)
    ## table <- .strip(x@.name, "\"")
    table <- .strip(table, "\"")
    l <- length(table)
    if (l == 2) {
        table.name <- table[2]
        table.schema <- table[1]
    } else {
        schemas <- arraydb.to.arrayr(
            db.q("select current_schemas(True)",
                 conn.id=conn.id, verbose = FALSE),
            type = "character")
        table_schema <- character(0)
        for (schema in schemas)
            if (.db.existsTable(c(schema, table), conn.id)) {
                table_schema <- c(table_schema, schema)
                break
            }
        table.name <- table
        table.schema <- table_schema
    }

    oid <- .db.getQuery(
        .format("SELECT c.oid
                 FROM pg_catalog.pg_class c
                 LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
                 WHERE c.relname ~ '^(<table.name>)$'
                 AND n.nspname ~ '^(<table.schema>)$'",
                 list(table.name=table.name,
                                      table.schema=table.schema)), conn.id)

    attrnums <- .db.getQuery(
        .format("SELECT attrnums
                 FROM pg_catalog.gp_distribution_policy t
                 WHERE localoid = '<oid>'", list(oid=as.numeric(oid))),
        conn.id = conn.id)
    attrnums <- as.integer(arraydb.to.arrayr(attrnums, "integer"))

    if (is.na(attrnums)) return (NA)

    cols <- .db.getQuery(
        .format("SELECT attname FROM pg_attribute
                 WHERE attrelid = '<oid>'
                 AND (<attnum>)",
                list(oid=oid,
                     attnum=paste("attnum='", attrnums, "'", sep = "",
                     collapse = " or "))), conn.id)[,,drop=TRUE]

    cols
}

## ----------------------------------------------------------------------

## get extra strings like where and sort etc
.get.extra.str <- function(data)
{
    if (is(data, "db.data.frame")) {
        tbl <- content(data)
        src <- tbl
        parent <- src
        where <- ""
        where.str <- ""
        sort <- list(by = "", order = "", str = "")
    } else {
        if (data@.source == data@.parent)
            tbl <- data@.parent
        else
            tbl <- paste("(", data@.parent, ") s", sep = "")
        src <- data@.source
        parent <- data@.parent
        where <- data@.where
        if (where != "") where.str <- paste(" where", where)
        else where.str <- ""
        sort <- data@.sort
    }

    list(tbl = tbl, where = where, where.str = where.str, sort = sort,
         src = src, parent = parent)
}

## ----------------------------------------------------------------------

setGeneric("rowSums")

.row.action <- function (x, action)
{
    x <- db.array(x)[,]
    ## Reduce(function(l,r) l+r, as.list(x))
    res <- x[,1]
    res@.expr <- paste(x@.expr, collapse = action)
    res@.content <- gsub("^select 1 as", paste("select", res@.expr, "as"),
                         res@.content)
    res
}

setMethod("rowSums",
    signature(x = "db.obj"),
    function (x, na.rm = FALSE, dims = 1, ...)
    {
        .row.action(x, "+")
    }
)

## ----------------------------------------------------------------------

setGeneric("rowMeans")

setMethod("rowMeans",
    signature(x = "db.obj"),
    function (x, na.rm = FALSE, dims = 1, ...)
    {
        rowSums(x) / length(names(x))
    }
)

## ----------------------------------------------------------------------

## combine a list of db.obj faster than using reduce
.combine.list <- function (lst)
{
    n <- length(lst)
    if (n == 1) return (lst[[1]])
    res <- NULL
    for (i in seq_len(n)) {
        if (is(lst[[i]], "db.data.frame")) {
            if (ncol(lst[[i]]) == 1 && lst[[i]]@.col.data_type == "array")
                lst[[i]] <- db.array(lst[[i]])
            else
                lst[[i]] <- lst[[i]][,]
        }
        if (is(lst[[i]], "db.obj") && is.null(res)) res <- lst[[i]]
    }

    ## res <- lst[[1]]
    res@.expr <- unlist(lapply(lst, function(x) {if (is(x, "db.obj"))
                                                     x@.expr else x}))
    res@.col.name <- unlist(
        lapply(lst, function(x) {
            if (is(x, "db.obj"))
                x@.col.name else
            sapply(seq_len(length(x)), function(i) .unique.string())}))
    res@.col.data_type <- unlist(lapply(lst, function(x) {
        if (is(x, "db.obj"))
            x@.col.data_type
        else {
            if (typeof(x) == "character") rep("text", length(x))
            else if (typeof(x) == "boolean")
                rep("boolean", length(x))
            else rep("double precision", length(x))
        }}))
    res@.col.udt_name <- unlist(lapply(lst, function(x) {
        if (is(x, "db.obj"))
            x@.col.udt_name
        else {
            if (typeof(x) == "character") rep("text", length(x))
            else if (typeof(x) == "boolean")
                rep("bool", length(x))
            else rep("float8", length(x))
        }}))
    res@.is.factor <- unlist(lapply(lst, function(x) {if (is(x, "db.obj"))
                             x@.is.factor else rep(FALSE, length(x))}))
    res@.factor.suffix <- unlist(
        lapply(lst, function(x) {
            if (is(x, "db.obj"))
                x@.factor.suffix else rep("", length(x))}))
    res@.is.agg <- unlist(
        lapply(lst, function(x) {
            if (is(x, "db.obj"))
                x@.is.agg else rep(FALSE, length(x))}))

    if (res@.source == res@.parent)
        tbl <- res@.parent
    else
        tbl <- "(" %+% res@.parent %+% ") s"
    where <- res@.where
    if (where != "") where.str <- paste(" where", where)
    else where.str <- ""
    sort <- res@.sort
    res@.content <- paste("select ",
                          paste(res@.expr, " as \"", res@.col.name, "\"",
                                collapse = ",", sep = ""),
                          " from ", tbl, where.str, sort$str, sep = "")
    res
}
