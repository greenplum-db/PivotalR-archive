
## Define UI for miles per gallon application
shinyUI(pageWithSidebar(

    ## Application title
    headerPanel("PivotalR"),
    
    ## Sidebar with controls to select the variable to plot against mpg
    ## and to specify whether outliers should be included
    sidebarPanel(
        uiOutput("conn.controls"),

        uiOutput("table.controls")
        ),
    
    ## Show the caption and plot of the requested variable against mpg
    mainPanel(
        verbatimTextOutput("con.info")
        )
    ))
