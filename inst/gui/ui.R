
## Define UI for miles per gallon application
shinyUI(pageWithSidebar(

    ## Application title
    headerPanel("PivotalR"),
    
    ## Sidebar with controls to select the variable to plot against mpg
    ## and to specify whether outliers should be included
    sidebarPanel(
        uiOutput("conn.controls"),

        selectInput("table", "Table", choices = "", selected = "")

        ## conditionalPanel(
        ##     condition = "output.tblSelected",
        ##     selectInput("model", "Model",
        ##                 choices = c("", "Linear Regression",
        ##                 "Logistic Regression"), selected = "")
        ##     )
        ),
    
    ## Show the caption and plot of the requested variable against mpg
    mainPanel(
        tableOutput("con.info"),

        tableOutput("tbl.info")
        )
    ))
