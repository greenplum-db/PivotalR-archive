## ----------------------------------------------------------------------
## Wrapper function for MADlib's kmeans functions
## ----------------------------------------------------------------------

madlib.kmeans <- function(
	x, centers, iter.max = 10, nstart = 1, algorithm = "Lloyd", key,
    fn.dist = "squared_dist_norm2", agg.centroid = "avg", min.frac = 0.001,
    kmeanspp = FALSE, seeding.sample.ratio=1.0, ...)
{
    .validate.input(x, iter.max, nstart, algorithm)

    conn.id <- conn.id(x) # connection ID
    warnings <- .suppress.warnings(conn.id)
    madlib.ver <- .madlib.version.number(conn.id)

    db <- .get.dbms.str(conn.id)
    madlib <- schema.madlib(conn.id) # MADlib schema name

    db.q("DROP TABLE IF EXISTS __madlib_temp_kmeans__",
        nrows = -1, conn.id = conn.id, verbose = FALSE)

    # Fix the input data

    expr <- names(x)
    expr <- expr[expr != key]

    # Collate columns other than key
    if (length(expr) > 1){

        tmp.x <- .collate.columns(x, expr)
        new.x.name <- .unique.string()
        db.q("create table ", new.x.name,
            " as (select ", key, ", __madlib_coll__ from ",
            content(tmp.x), ")", sep="", verbose = FALSE)
        x <- db.data.frame(new.x.name, conn.id=conn.id, verbose=FALSE)
        expr <- "__madlib_coll__"
    }

    tbl.source <- content(x)
    tbl.fn.dist <- paste(madlib,".",fn.dist, sep="")
    tbl.agg <- paste(madlib,".", agg.centroid, sep="")

    cl <- class(centers)
    if(cl == "numeric") { # just the number of centroids

        center.count <- centers
        if (kmeanspp){
            km.func <- "kmeanspp"
        }else{
            km.func <- "kmeans_random"
        }

        sql_from <- paste(
            madlib, ".", km.func, "('", tbl.source, "','",
            expr, "',", centers, ",'", tbl.fn.dist, "','", tbl.agg, "',",
            iter.max, ",", min.frac, sep="")

        if (kmeanspp){
            sql_from <- paste(sql_from, ",", seeding.sample.ratio, ")", sep="")
        } else{
            sql_from <- paste(sql_from, ")", sep="")
        }

        obj <- .Machine$double.xmax
        obji <- 0

        for (i in 1:nstart){

            db.q(paste(
                "DROP TABLE IF EXISTS __madlib_temp_kmeans__",i,"__",
                sep="" ), nrows = -1, conn.id = conn.id, verbose = FALSE)

            sql_i <- paste("CREATE TABLE __madlib_temp_kmeans__",i,
                "__ AS SELECT * FROM ", sql_from, sep="")

            db.q(sql_i, nrows = -1, conn.id = conn.id, verbose = FALSE)

            res.i <- db.q(paste(
                "SELECT * FROM __madlib_temp_kmeans__",i,"__",sep=""
                ), nrows = -1, conn.id = conn.id, verbose = FALSE)

            if (res.i$objective_fn < obj){
                obj <- res.i$objective_fn
                obji <- i
            }
        }

        sql_km <- paste(
            "CREATE TABLE __madlib_temp_kmeans__ AS SELECT * FROM",
            " __madlib_temp_kmeans__",obji,"__", sep="")

        db.q(sql_km, nrows = -1, conn.id = conn.id, verbose = FALSE)

        lapply(1:nstart, FUN=function(i) {
            db.q(paste(
                "DROP TABLE IF EXISTS __madlib_temp_kmeans__",i,"__",
                sep="" ), nrows = -1, conn.id = conn.id, verbose = FALSE)})

    } else {
        if (cl == "db.table") {

            centers.name <- content(centers)
            center.count <- nrow(centers)

            # Fix the centers table
            c.expr <- names(centers)
            if (c.expr > 1){

                tmp.c <- .collate.columns(centers, c.expr)
                centers.name <- .unique.string()
                db.q("create table ", centers.name,
                    " as (select __madlib_coll__ from ",
                    content(tmp.c), ")", sep="", verbose = FALSE)
                centers <- paste(centers.name, "','__madlib_coll__")

            } else{

                centers <- paste(centers.name,"','",names(centers)[1])
            }

        } else if (class(centers) == "matrix"){

            center.count <- nrow(centers)
            # Matrix to 2d array conversion
            # y<-apply(centers, 1, paste, collapse =",", sep="")
            # z<-paste("{",y,"}")
            # t<-paste(z, sep="", collapse=",")
            # paste("{",t,"}")

            # This process can be turned into a loop for any n dim. array
            centers <- paste("{", paste( paste( "{",
                apply( centers, 1, paste, collapse =",", sep=""), "}", sep=""),
                sep="", collapse=","), "}", sep="")
        } else {

            stop("madlib.kmeans could not recognize the centers parameter")
        }

        sql_km <- paste(
            "CREATE TABLE __madlib_temp_kmeans__ AS select * from ",
            madlib, ".kmeans('", tbl.source, "','",
            expr, "','", centers, "','",
            tbl.fn.dist, "','", tbl.agg, "',",
            iter.max, ",", min.frac, ")", sep="")

        db.q(sql_km, nrows = -1, conn.id = conn.id, verbose = FALSE)
    }


    res.raw <- db.q("SELECT * FROM __madlib_temp_kmeans__",
    	nrows = -1, conn.id = conn.id, verbose = FALSE)

    # Find the closest columns for the clustering vector
    sql_cl <- paste(
    	"SELECT data.", key, ", (" ,madlib ,
    	".closest_column(centroids, data.",expr,")).column_id ",
    	" AS cluster_id FROM ", tbl.source, " AS data, ",
    	"(SELECT centroids FROM __madlib_temp_kmeans__) as centroids ",
    	" ORDER BY ", key,sep="")

    cluster <- db.q(sql_cl, nrows = -1, conn.id = conn.id, verbose = FALSE)

    # MADlib cluster ids start from 0, R start from 1
    res.cluster <- cluster$cluster_id+1

 	res.tot.withinss <- res.raw$objective_fn
 	res.size <- as.vector(table(res.cluster))
    res.iter <- res.raw$num_iterations

    res.centers <- matrix(arraydb.to.arrayr(res.raw$centroids),
        nrow=center.count, byrow=TRUE)

    rownames(res.centers) <- names(table(res.cluster))
    mlver <- strsplit(madlib.ver,".",TRUE)

    if (as.integer(mlver[[1]][2]) > 9 || as.integer(mlver[[1]][1]) > 1){
        res.withinss = as.vector(arraydb.to.arrayr(res.raw$cluster_variance))
    } else {
        res.withinss = NULL
        warning("MADlib version is lower than 1.10.0",
            "Some output fields might be missing.")
    }

    ret <- list( cluster=res.cluster, centers=res.centers,
        withinss=res.withinss, tot.withinss=res.tot.withinss,
        size = res.size, iter=res.iter)

    ret <- structure(ret, class="kmeans")
    .restore.warnings(warnings)
    ret
}

.validate.input <- function( x, iter.max, nstart, algorithm, ...)
{

    if (! is(x, "db.obj"))
        stop("madlib.kmeans can only be used on a db.obj object, and ",
             deparse(substitute(x)), " is not!")
    if (!is(iter.max, "numeric") || iter.max < 0)
        stop("madlib.kmeans iter.max has to be a positive numeric value",
             deparse(substitute(iter.max)), " is not!")
    if (!is(nstart, "numeric") || nstart < 0)
        stop("madlib.kmeans nstart has to be a positive numeric value",
             deparse(substitute(nstart)), " is not!")
    if (algorithm != "Lloyd")
        warning("madlib.kmeans algorithm has to be a Lloyd",
             deparse(substitute(algorithm)), " is not!")

}
