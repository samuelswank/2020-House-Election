library(shiny)
library(tidyverse)
source("census.R")

ui <- fluidPage(
  fluidRow(
      column(4, selectInput("selectedState", "State", choices = state.name)),
      column(4, uiOutput("selectedDistrict"))
  )
)

server <- function(input, output, session) {
  output$selectedDistrict <- renderUI({
    selectInput("selectedDistrict", "District", choices = districtChoices[[input$selectedState]])
  })
    # output$stateMap <- renderPlot({
    #   plotState(
    #     stateData(input$selectedDistrict, stateString(input$selectedDistrict)),
    #     input$selectedDistrict
    #     )
    # })
    # 
    # output$districtMap <- renderPlot({
    #   plotDistrict(
    #     stateData(input$selectedDistrict, stateString(input$selectedDistrict)),
    #     input$selectedDistrict
    #     )
    # })
}

shinyApp(ui, server)