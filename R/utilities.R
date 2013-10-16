
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

    list(train = train, valid = valid, inter = y)
}

## ----------------------------------------------------------------------

## suppress all warnings
.suppress.warnings <- function (conn.id)
{
    msg.level <- .set.msg.level("panic", conn.id = conn.id)
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
