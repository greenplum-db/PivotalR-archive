
.get.connection.list <- function ()
{
    id <- PivotalR:::.localVars$conn.id[,1]
    as.character(id)
}

## -----------------------------------------------------------------------

## Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
    ## Return the requested dataset
    conInput <- reactive({
        as.integer(input$connection)
    })

    output$conn.controls <- renderUI({
        selectInput("connection", "Connection:",
                    choices = .get.connection.list())
    })

    output$table.controls <- renderUI({
        selectInput("tables", "Tables:",
                    choices = c("", db.objects(conn.id = conInput())),
                    selected = "")
    })

    output$con.info <- renderPrint({
       conn.id <- conInput()
       conn.id
    })
})

