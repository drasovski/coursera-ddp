suppressPackageStartupMessages(library(shiny))

library(shiny)

shinyUI(fluidPage(

    titlePanel("Quick Data Set Explorer"),

    sidebarLayout(
        sidebarPanel(
            # data set selection UI
            uiOutput("datasetui"),
            # variable selection UI
            uiOutput("onevarui"),
            uiOutput("twovar1ui"),
            uiOutput("twovar2ui")
    ),

    mainPanel(
        tabsetPanel(
            # tabs for separate output display
            tabPanel("About the Data",
                     htmlOutput("about")),
            tabPanel("Summary of Data",
                     dataTableOutput("summary")),
            tabPanel("One Variable Explorer",
                     plotOutput("histogram"),
                     plotOutput("boxplot")),
            tabPanel("Two Variable Explorer",
                     plotOutput("scatterplot"))
        )
    )
  )
))
