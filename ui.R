library(shiny)
library(markdown)

shinyUI(pageWithSidebar(
        headerPanel("Eurowaternet Data Analysis"),
        sidebarPanel(
                h3('Data Range:'),
                selectInput("station_id", "Station:",
                            listStation),
                selectInput("depth", "Depth:", 
                            listDepth),
                h3('Time Series:'),
                selectInput("variable", "Variable:",
                            listVar, selected="T"),
                h3('Regression:'),
                selectInput("varX", "Variable (X):",
                            listVar, selected="T"),
                selectInput("varY", "Variable (Y):",
                            listVar, selected="SAL"),
                actionButton("simpModButton", "Simple linear model"),
                actionButton("bestModButton", "Best linear model"),
                width = 3
        ),
        mainPanel(
                tabsetPanel(
                        tabPanel("Exploratory analysis", plotOutput("timeSeriesPlot")), 
                        tabPanel("Regression", plotOutput("regressionPlot"),
                                 verbatimTextOutput("text1")),
                        tabPanel("Help", includeMarkdown("readme.md"))
                )
        )
))

