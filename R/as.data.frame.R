as.data.frame.db.table <-
    as.data.frame.db.view <-
    as.data.frame.db.Rquery <- function(
        x, nrows=100, row.names=NULL, optional=FALSE, 
        stringsAsFactors=default.stringsAsFactors(),
        array=TRUE, ...)
{
    df <- lookat(x, nrows=nrows, array=array)
    if(stringsAsFactors && any(sapply(df, "class") == "character"))
        df[] <- lapply(df, function(x) if(is.character(x)) factor(x) else x)
    df
}
