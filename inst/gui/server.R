
.get.connection.list <- function ()
{
    id <- PivotalR:::.localVars$conn.id[,1]
    as.character(id)
}

## ------------------------------------------------------------------------

.get.connection.info <- function (conn.id)
{
    if (identical(conn.id, integer(0)))
        data.frame(Host = "", User = "", Database = "", DBMS = "")
    else {
        idx <- PivotalR:::.localVars$conn.id[PivotalR:::.localVars$conn.id[,1] == conn.id, 2]
        db <- PivotalR:::.get.dbms.str(conn.id)
        data.frame(Host = PivotalR:::.localVars$db[[idx]]$host,
                   User = PivotalR:::.localVars$db[[idx]]$user,
                   Database = PivotalR:::.localVars$db[[idx]]$dbname,
                   DBMS = paste(db$db.str, db$version.str))
    }
}

## -----------------------------------------------------------------------

## Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
    ## Return the requested dataset
    conInput <- reactive({
        as.integer(input$connection)
    })

    tblInput <- reactive({
        input$table
    })

    db.obj.list <- reactive({
        db.objects(conn.id = conInput())
    })

   
    output$conn.controls <- renderUI({
        selectInput("connection", "Connection",
                    choices = .get.connection.list())
    })

    observe({
        id <- input$connection
        if (!is.null(id)) {
            db.objs <- db.objects(conn.id = as.integer(id))
            updateSelectInput(session, "table", label = "Table",
                              choices = c("", db.objs),
                              selected = "")
        }
    })

    output$con.info <- renderTable({
       conn.id <- conInput()
       .get.connection.info(conn.id)
    })

    output$tblSelected <- reactive({
        tbl <- tblInput()
        if (!is.null(tbl) && tbl != "") 1
        else 0
    })

    output$tbl.info <- renderTable({
        conn.id <- conInput()
        tbl <- tblInput()
        if (is.null(tbl) || tbl == "") {
            data.frame()
        } else {
            x <- db.data.frame(tbl, conn.id = conn.id, verbose = FALSE)
            res <- madlib.summary(x)
            class(res) <- "data.frame"
            res
        }
    })
})

