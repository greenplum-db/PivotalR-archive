## Useful math functions

setMethod ("sin",
           signature(x = "db.obj"),
           function (x)
           .aggregate(x, "sin", FALSE, .num.types, TRUE,
                      x@.col.data_type, x@.col.udt_name),
           valueClass = "db.Rquery")

## ----------------------------------------------------------------------

setMethod ("cos",
           signature(x = "db.obj"),
           function (x)
           .aggregate(x, "cos", FALSE, .num.types, TRUE,
                      x@.col.data_type, x@.col.udt_name),
           valueClass = "db.Rquery")

## ----------------------------------------------------------------------

setMethod ("tan",
           signature(x = "db.obj"),
           function (x)
           .aggregate(x, "tan", FALSE, .num.types, TRUE,
                      x@.col.data_type, x@.col.udt_name),
           valueClass = "db.Rquery")

## ----------------------------------------------------------------------

setMethod ("asin",
           signature(x = "db.obj"),
           function (x)
           .aggregate(x, "asin", FALSE, .num.types, TRUE,
                      x@.col.data_type, x@.col.udt_name),
           valueClass = "db.Rquery")

## ----------------------------------------------------------------------

setMethod ("acos",
           signature(x = "db.obj"),
           function (x)
           .aggregate(x, "acos", FALSE, .num.types, TRUE,
                      x@.col.data_type, x@.col.udt_name),
           valueClass = "db.Rquery")

## ----------------------------------------------------------------------

setMethod ("atan",
           signature(x = "db.obj"),
           function (x)
           .aggregate(x, "atan", FALSE, .num.types, TRUE,
                      x@.col.data_type, x@.col.udt_name),
           valueClass = "db.Rquery")

## ----------------------------------------------------------------------

setGeneric("atan2")

setMethod ("atan2",
           signature(y = "db.obj", x = "db.obj"),
           function (y, x) atan(y/x),
           valueClass = "db.Rquery")

setMethod ("atan2",
           signature(y = "numeric", x = "db.obj"),
           function (y, x) atan(y/x),
           valueClass = "db.Rquery")

setMethod ("atan2",
           signature(y = "db.obj", x = "numeric"),
           function (y, x) atan(y/x),
           valueClass = "db.Rquery")
