library(shiny)
library(plotly)
library(ggplot2)

shinyUI(
    navbarPage("Next Word Prediction App",
        tabPanel("Prediction",
            sidebarLayout(
        
                sidebarPanel(
                    textInput("data","Write your sentence", value="At the top of"),
                    sliderInput("top",
                        "Number of predictions:",
                        min = 1,
                        max = 30,
                        value = 3)
                ),
                mainPanel(
                    verbatimTextOutput("next_word"),
                    br(),
                    br(),
                    plotlyOutput("Plot_word"),
                    br(),
                    br(),
                    dataTableOutput("table")
                )
            )
        ),
        tabPanel("N-grams"
                 ,               
           mainPanel(
                dataTableOutput("table_stats")
            )
        )
    )
)
