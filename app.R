library(shiny)
library(tidyverse)
source("stringManipulator.R")
source("census.R")
source("map.R")

stateChoices <- state.name %>% R.utils::insert(1, c(""))
'%!in%' <- function(x,y){!('%in%'(x,y))}

ui <- fluidPage(
  fluidRow(
    column(
      4, 
      selectInput("selectedState", "State", choices = stateChoices)
      ),
    column(4, uiOutput("selectedDistrict"))
  ),
  fluidRow(
    column(4, plotOutput("stateMap")), column(4, plotOutput("districtMap"))
    )
)


server <- function(input, output, session) {
  output$selectedDistrict <- renderUI({
    selectInput(
      "selectedDistrict", "District", districtChoices[[input$selectedState]]
      )
  })
  
  # output$stateMap <- renderPlot({plotState(input$selectedState)})
  
  observeEvent(input$selectedState, {
    if (input$selectedState == "") {output$stateMap <- NULL}
    else if (input$selectedState %!in% atLarge) {
      output$stateMap <- renderPlot({plotState(input$selectedState)})
      output$districtMap <- renderPlot({
        plotDistrict(input$selectedState, input$selectedDistrict)
      })
    } else if (input$selectedState %in% atLarge) {
      output$stateMap <- renderPlot({plotState(input$selectedState)})
      output$districtMap <- NULL
    }
  })
}

shinyApp(ui, server)