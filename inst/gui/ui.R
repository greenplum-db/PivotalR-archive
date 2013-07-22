
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
            selectInput("model", "Select a model/operation:",
                        choices = c("", "Linear Regression",
                        "Logistic Regression"), selected = "")),

        conditionalPanel(
            condition = "input.model != '' && input.table != ''",
            checkboxInput("formula", "Use R formula", value = FALSE)),

        conditionalPanel(
            condition = "input.model != '' && input.formula == false && input.table != ''",
            uiOutput("dep.controls")),

        conditionalPanel(
            condition = "input.model != '' && input.formula == false && input.table != ''",
            uiOutput("ind.controls")),

        conditionalPanel(
            condition = "input.model != '' && input.formula == false && input.table != ''",
            uiOutput("grp.controls")),

        conditionalPanel(
            condition = "input.model != '' && input.formula == true && input.table != ''",
            textInput("rformula", "Input the formula for the model:"))
        ),
    
    ## Show the caption and plot of the requested variable against mpg
    mainPanel(
        tabsetPanel(
            tabPanel("Summary",
                     h4("Database connection"),
                     tableOutput("con.info"),
                     conditionalPanel(condition = "input.table != ''",
                                      h4("Table summary")),
                     tableOutput("tbl.info")),
            tabPanel("Computation",
                     verbatimTextOutput("model.info")),
            id = "tabset"            
        )
    )
    ))
