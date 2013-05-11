
## ------------------------------------------------------------------------
## Some operations for Arith and Compare
## 6 Compare operation methods, 7 Arith operation methods
##
## For each operation, we need to create methods for signatures
## db.data.frame and db.data.frame
## db.Rquery and db.Rquery,
## db.Rquery and numeric, numeric and db.Rquery
## db.Rquery and character, character and db.Rquery
##
## That would be 13 x 6 = 78 methods! How to avoid typing so many
## functions? But of course, they are small functions, and can be
## done in an hour. However, this would bring lots of redundant
## things into the manual.
## ------------------------------------------------------------------------

setMethod ("==",
           c("db.data.frame", "db.data.frame"),
           function (e1, e2) {
               if (all(e1@.name == e1@.name) &&
                   e1@.content == e2@.content &&
                   conn.eql(e1@.conn.id, e2@.conn.id) &&
                   e1@.table.type == e2@.table.type)
                   TRUE
               else
                   FALSE
           },
           valueClass = "logical")

setMethod ("!=",
           c("db.data.frame", "db.data.frame"),
           function (e1, e2) {
               if (e1 == e2)
                   FALSE
               else
                   TRUE
           },
           valueClass = "logical")

## setMethod ("==",
##            c("db.Rquery", "db.Rquery"),
##            function(e1, e2) {
##                e1@x != e2@x | e1@y != e2@y
##            },
##            valueClass = "db.Rquery")
