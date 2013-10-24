as.data.frame.db.table <-
    as.data.frame.db.view <-
    as.data.frame.db.Rquery <- function(
        x, row.names=NULL, optional=FALSE, nrows=NULL,
        stringsAsFactors=default.stringsAsFactors(),
        array=TRUE, ...)
{
    df <- lookat(x, nrows=nrows, array=array, drop=FALSE)
    if (is.data.frame(df) && stringsAsFactors && any(sapply(df, is.character)))
        df[] <- lapply(df, function(x) if(is.character(x)) factor(x) else x)
    df
}
