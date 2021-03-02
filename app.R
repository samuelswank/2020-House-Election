library(shiny)
library(tidyverse)
source("helpers.R")
source("stringManipulator.R")
source("map.R")

stateChoices <- state.name %>% R.utils::insert(1, c(""))
'%!in%' <- function(x,y){!('%in%'(x,y))}

ui <- fluidPage(
  # Title
  fluidRow(column(12, centerText(h1("2020 House Elections Model")))),
  # Dropdown Menu Row
  fluidRow(
    column(3),
    column(
      3, 
      selectInput("selectedState", "State", choices = stateChoices)
      ),
    column(3, uiOutput("selectedDistrict"))
  ),
  # Actual Row
  fluidRow(
    column(2),
    column(4, plotOutput("stateMap")),
    column(4, plotOutput("districtMap")),
    column(2)
    ),
  # Predicted Row
  fluidRow(
    column(2),
    column(4, plotOutput("predictedState")),
    column(4, plotOutput("predictedDistrict")),
    column(2)
    ),
  fluidRow(column(12, uiOutput("statistics"))),
  fluidRow(
    column(1),
    column(3, uiOutput("importancesTitle"), tableOutput("importancesTable"))
    )
)


server <- function(input, output, session) {
  output$selectedDistrict <- renderUI({
    selectInput(
      "selectedDistrict", "District", districtChoices[[input$selectedState]]
      )
  })
  
  statistics <- centerText(h2("Statistics"))
  
  # Party Affiliation Model Maps
  observeEvent(input$selectedState, {
    if (input$selectedState == "") {
      output$stateMap     <- NULL
      output$districtMap  <- NULL
    } else if (input$selectedState %!in% atLarge) {
      output$stateMap <- renderPlot({plotState(input$selectedState)})
      output$districtMap <- renderPlot({
        plotDistrict(input$selectedState, input$selectedDistrict)
        })
      output$predictedState <- renderPlot({
        plotPredicted(input$selectedState, NA, geography = "state")
        })
      output$predictedDistrict <- renderPlot({
        plotPredicted(
          input$selectedState, input$selectedDistrict, geography = "district"
          )
      })
      output$statistics <- renderUI({statistics})
      output$importancesTitle <- renderUI({centerText(h3("Importances"))})
      output$importancesTable <- renderTable(topTen, rownames = TRUE)
    } else if (input$selectedState %in% atLarge) {
      output$stateMap <- renderPlot({plotState(input$selectedState)})
      output$districtMap <- renderPlot({plotState(input$selectedState)})
      output$predictedState <- renderPlot({
        plotPredicted(input$selectedState, NA, geography = "state")
      })
      output$predictedDistrict <- renderPlot({
        plotPredicted(input$selectedState, NA, geography = "state")
      })
      output$statistics <- renderUI({statistics})
      output$importancesTitle <- renderUI({centerText(h3("Importances"))})
      output$importancesTable <- renderTable(topTen, rownames = TRUE)
    }
  })
  
}

shinyApp(ui, server)