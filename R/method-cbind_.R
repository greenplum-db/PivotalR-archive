## method for cbind2

setMethod ("cbind2",
           signature (x = "db.obj", y = "db.obj"),
           def = function (x, y) {
               z <- x
               for (name in names(y)) {
                   if (! (name %in% names(z)))
                       z[[name]] <- y[[name]]
                   else {
                       count <- 1
                       while (paste(name, ".", count, sep = "") %in% names(z))
                           count <- count + 1
                       z[[paste(name, ".", count, sep = "")]] <- y[[name]]
                   }
               }
               z
           },
           valueClass = "db.Rquery")

## ----------------------------------------------------------------------

setGeneric("cbind",
           function(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`,
                    ..., deparse.level = 1)
           {
               no.first <- missing(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`)
               has.db.obj <- FALSE
               for (arg in c(...))
                   if (is(arg, "db.obj")) {
                       has.db.obj <- TRUE
                       break
                   }
               if (no.first && !has.db.obj ||
                   (!no.first &&
                    !is(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`,
                        "db.obj") && !has.db.obj))
               {
                   if (no.first)
                       args <- list(..., deparse.level = deparse.level)
                   else {
                       args <- list(
                                    `__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`, ...,
                                    deparse.level = deparse.level)
                   }
                   do.call(base::cbind, args)
               } else {
                   if (no.first)
                       lst <- c(...)
                   else
                       lst <- c(`__madlib_temp_27836b51_49d3_9d9a96_dc459ab0ef62__`,
                                ...)
                   .combine.list(lst)
               }
           })

## ----------------------------------------------------------------------

## .find.pos <- function(str)
## {
##     helper <- function(..) 1
##     str <- strsplit(str, ",")[[1]]
## 
## }
