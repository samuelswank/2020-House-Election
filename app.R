library(shiny)
library(tigris)
library(tidyverse)
source("houseResults.R")
source("map.R")

ui <- fluidPage(
  fluidRow(
      column(
          4, 
          selectInput(
              "selectedDistrict", 
              "District", 
              choices = districtChoices,
              selected = "Utah District 4"
              )
          )
  ),
  fluidRow(
      column(6, plotOutput("stateMap")),
      column(6, plotOutput("districtMap"))
  )
)

server <- function(input, output, session) {
    output$stateMap <- renderPlot({
      plotState(
        stateData(input$selectedDistrict, stateString(input$selectedDistrict)),
        input$selectedDistrict
        )
    })
    
    output$districtMap <- renderPlot({
      plotDistrict(
        stateData(input$selectedDistrict, stateString(input$selectedDistrict)),
        input$selectedDistrict
        )
    })
}

shinyApp(ui, server)