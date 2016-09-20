## ----------------------------------------------------------------------
## Wrapper function for MADlib's kmeans functions
## ----------------------------------------------------------------------

setClass("kmeans.madlib")

madlib.kmeans <- function(
	x, centers, iter.max = 10, nstart = 1, algorithm = "Lloyd",
	pid, expr, expr.centroid = NULL,
    fn.dist = "squared_dist_norm2", agg.centroid = "avg", min.frac = 0.001,
    kmeanspp = FALSE, seeding.sample.ratio=1.0, ...)
{
    .validate.input(x, iter.max, nstart, algorithm)

    conn.id <- conn.id(x) # connection ID
    warnings <- .suppress.warnings(conn.id)
    madlib.ver <- .madlib.version.number(conn.id)

    db <- .get.dbms.str(conn.id)
    madlib <- schema.madlib(conn.id) # MADlib schema name

    tbl.source <- content(x)
    tbl.fn.dist <- paste(madlib,".",fn.dist, sep="")
    tbl.agg <- paste(madlib,".", agg.centroid, sep="")

    db.q("DROP TABLE IF EXISTS __madlib_pivotalr_kmeans__",
        nrows = -1, conn.id = conn.id, verbose = FALSE)

    if(class(centers) == "numeric") { # just the number of centroids

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
                "DROP TABLE IF EXISTS __madlib_pivotalr_kmeans__",i,"__",
                sep="" ), nrows = -1, conn.id = conn.id, verbose = FALSE)

            sql_i <- paste("CREATE TABLE __madlib_pivotalr_kmeans__",i,
                "__ AS SELECT * FROM ", sql_from, sep="")

            db.q(sql_i, nrows = -1, conn.id = conn.id, verbose = FALSE)

            res.i <- db.q(paste(
                "SELECT * FROM __madlib_pivotalr_kmeans__",i,"__",sep=""
                ), nrows = -1, conn.id = conn.id, verbose = FALSE)

            if (res.i$objective_fn < obj){
                obj <- res.i$objective_fn
                obji <- i
            }
        }

        sql_km <- paste(
            "CREATE TABLE __madlib_pivotalr_kmeans__ AS SELECT * FROM
            __madlib_pivotalr_kmeans__",obji,"__", sep="")

        db.q(sql_km, nrows = -1, conn.id = conn.id, verbose = FALSE)

        lapply(1:nstart, FUN=function(i) {
            db.q(paste(
                "DROP TABLE IF EXISTS __madlib_pivotalr_kmeans__",i,"__",
                sep="" ), nrows = -1, conn.id = conn.id, verbose = FALSE)})

    }else{
        if (!is.null(expr.centroid)){
            center.count <- db.q(
                paste("SELECT count(*) FROM ", centers, sep=""),
                nrows = -1, conn.id = conn.id, verbose = FALSE)$count

            centers <- paste(centers,"','",expr.centroid)
            print(center.count)
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
        }

        sql_km <- paste(
            "CREATE TABLE __madlib_pivotalr_kmeans__ AS select * from ",
            madlib, ".kmeans('", tbl.source, "','",
            expr, "','", centers, "','",
            tbl.fn.dist, "','", tbl.agg, "',",
            iter.max, ",", min.frac, ")", sep="")

        db.q(sql_km, nrows = -1, conn.id = conn.id, verbose = FALSE)
    }


    res.raw <- db.q("SELECT * FROM __madlib_pivotalr_kmeans__",
    	nrows = -1, conn.id = conn.id, verbose = FALSE)

    # Find the closest columns for the clustering vector
    sql_cl <- paste(
    	"SELECT data.", pid, ", (" ,madlib ,
    	".closest_column(centroids, data.",expr,")).column_id ",
    	" AS cluster_id FROM ", tbl.source, " AS data, ",
    	"(SELECT centroids FROM __madlib_pivotalr_kmeans__) as centroids ",
    	" ORDER BY ", pid,sep="")

    cluster <- db.q(sql_cl, nrows = -1, conn.id = conn.id, verbose = FALSE)

    # MADlib cluster ids start from 0, R start from 1
    res.cluster <- cluster$cluster_id+1


 	res.tot.withinss <- res.raw$objective_fn
 	res.size <- as.vector(table(res.cluster))
    res.iter <- res.raw$num_iterations

    res.centers <- matrix(arraydb.to.arrayr(res.raw$centroids),
        nrow=center.count, byrow=TRUE)

    rownames(res.centers) <- names(table(res.cluster))

	ret <- list(cluster=res.cluster, centers=res.centers,
		tot.withinss=res.tot.withinss, size = res.size, iter=res.iter)

    if (madlib.ver > "1.9.1"){
        c(ret, withinss=as.vector(
            arraydb.to.arrayr(res.raw$cluster_variance)))
    }

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
