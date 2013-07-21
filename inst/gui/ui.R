
## Define UI for miles per gallon application
shinyUI(pageWithSidebar(

    ## Application title
    headerPanel("PivotalR"),
    
    ## Sidebar with controls to select the variable to plot against mpg
    ## and to specify whether outliers should be included
    sidebarPanel(
        uiOutput("conn.controls"),

        uiOutput("tbl.controls"),
        
        ## selectInput("table", "Table", choices = "", selected = ""),

        conditionalPanel(
            condition = "input.table != ''",
            selectInput("model", "Model",
                        choices = c("", "Linear Regression",
                        "Logistic Regression"), selected = "")),

        conditionalPanel(
            condition = "input.model != ''",
            uiOutput("dep.controls")),

        conditionalPanel(
            condition = "input.model != ''",
            uiOutput("ind.controls"))
        ),
    
    ## Show the caption and plot of the requested variable against mpg
    mainPanel(
        tabsetPanel(
            tabPanel("Summary",
                     tableOutput("con.info"),
                     tableOutput("tbl.info")),
            tabPanel("Model",
                     verbatimTextOutput("model.info"))
            
        )
    )
    ))
