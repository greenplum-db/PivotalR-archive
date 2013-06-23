
## ------------------------------------------------------------------------
## Array operation utilities
## ------------------------------------------------------------------------

## get each element in an array expression
## returns an array of string
.get.array.elements <- function (expr, tbl, where.str, conn.id)
{
    s <- gsub("array\\[(.*)\\]", "\\1", expr)
    if (s == expr) {
        n <- as.integer(.db.getQuery(paste0(
            "select array_upper(\"", s, "\", 1) - array_lower(\"",
            s, "\", 1) + 1 from ",
            tbl, where.str, " limit 1"), conn.id))
        paste("\"", s, "\"[", seq_len(n), "]", sep = "")
    } else {
        regmatches(s, gregexpr("\\([^(\\),)]*\\)", s, perl=T))[[1]]
    }
}
