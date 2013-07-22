
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

    db.obj.list <- reactive({
        id <- conInput()
        if (identical(id, integer(0)) ||
            !PivotalR:::.is.conn.id.valid(id))
            ""
        else
            db.objects(conn.id = id)
    })

    ## ------------------------------------------------
    
    output$conn.controls <- renderUI({
        selectInput("connection", "Select a connection",
                    choices = .get.connection.list())
    })

    ## observe({
    ##     id <- input$connection
    ##     if (!is.null(id)) {
    ##         db.objs <- db.objects(conn.id = as.integer(id))
    ##         updateSelectInput(session, "table", label = "Table",
    ##                           choices = c("", db.objs),
    ##                           selected = "")
    ##     }
    ## })

    output$tbl.controls <- renderUI({
        selectInput("table", "Select a table:",
                    choices = c("", db.obj.list()),
                    selected = "")
    })

    output$dep.controls <- renderUI({
        id <- conInput()
        tbl <- input$table
        if (identical(id, integer(0)) || is.null(tbl) || tbl == "")
            vars <- c("")
        else
            vars <- c("", names(db.data.frame(tbl, conn.id = id,
                                              verbose = FALSE)))
        selectInput("dep", "Select the dependent Variable:",
                    choices = vars,
                    selected = "")
    })

    output$ind.controls <- renderUI({
        id <- conInput()
        tbl <- input$table
        if (identical(id, integer(0)) || is.null(tbl) || tbl == "")
            vars <- c("")
        else
            vars <- names(db.data.frame(tbl, conn.id = id,
                                        verbose = FALSE))
        checkboxGroupInput("ind", "Select the independent Variables:",
                           choices = vars)
    })

    output$grp.controls <- renderUI({
        id <- conInput()
        tbl <- input$table
        if (identical(id, integer(0)) || is.null(tbl) || tbl == "")
            vars <- c("")
        else
            vars <- names(db.data.frame(tbl, conn.id = id,
                                        verbose = FALSE))
        checkboxGroupInput("grp", "Select the grouping variables:",
                           choices = vars)
    })

    observe({
        if (is.null(input$model) || input$model == "" ||
            input$table == "")
            updateTabsetPanel(session, "tabset", selected = "Summary")
        else
            updateTabsetPanel(session, "tabset", selected = "Computation")
    })

    ## observe({
    ##     if (is.null(input$table) || input$table == "")
    ##         textInput("model", "Update model value", value = "")
    ## })    
    
    ## ------------------------------------------------
    
    output$con.info <- renderTable({
       conn.id <- conInput()
       .get.connection.info(conn.id)
    })

    output$tbl.info <- renderTable({
        conn.id <- conInput()
        tbl <- input$table
        if (is.null(tbl) || tbl == "") {
            data.frame()
        } else {
            x <- db.data.frame(tbl, conn.id = conn.id, verbose = FALSE)
            res <- madlib.summary(x)
            class(res) <- "data.frame"
            res[,-c(1,2)]
        }
    })

    output$model.info <- renderPrint({
        conn.id <- conInput()
        tbl <- input$table
        dep <- input$dep
        ind <- input$ind
        grp <- input$grp
        fml <- input$rformula
        
        empty.res <- "No model"
        class(empty.res) <- "none.obj"
    
        if (is.null(tbl) || tbl == "" ||
            ((is.null(dep) || is.null(ind)) && fml == "")) {
            empty.res
        } else {
            x <- db.data.frame(tbl, conn.id = conn.id, verbose = FALSE)
            if (fml == "") {
                f <- paste(dep, "~", paste(ind, collapse = " + "))
                if (!is.null(grp))
                    f <- paste(f, "|", paste(grp, collapse = " + "))
            } else
                f <- fml
            cat("\nf =", f, "\n--------------------------------------\n")
            if (input$model == "Linear Regression")
                res <- madlib.lm(formula(f), data = x)
            else if (input$model == "Logistic Regression")
                res <- madlib.glm(formula(f), data = x,
                                  family = "binomial")
            res
        }
    })
})

