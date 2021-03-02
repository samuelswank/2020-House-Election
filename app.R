library(shiny)
library(tidyverse)
source("stringManipulator.R")
source("map.R")

stateChoices <- state.name %>% R.utils::insert(1, c(""))
'%!in%' <- function(x,y){!('%in%'(x,y))}

ui <- fluidPage(
  # Dropdown Menu Row
  fluidRow(
    column(
      6, 
      selectInput("selectedState", "State", choices = stateChoices)
      ),
    column(6, uiOutput("selectedDistrict"))
  ),
  # Actual Row
  fluidRow(
    column(6, plotOutput("stateMap")), column(6, plotOutput("districtMap"))
    ),
  # Predicted Row
  fluidRow(
    column(6, plotOutput("predictedState")),
    column(6, plotOutput("predictedDistrict"))
    )
)


server <- function(input, output, session) {
  output$selectedDistrict <- renderUI({
    selectInput(
      "selectedDistrict", "District", districtChoices[[input$selectedState]]
      )
  })
  
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
    } else if (input$selectedState %in% atLarge) {
      output$stateMap <- renderPlot({plotState(input$selectedState)})
      output$districtMap <- NULL
      output$predictedState <- renderPlot({
        plotPredicted(input$selectedState, NA, geography = "state")
      })
      output$predictedDistrict <- NULL
    }
  })
}

shinyApp(ui, server)