library(shiny)
library(tidycensus)
library(tigris)
library(tidyverse)
source("dataWrangling/houseResults.R")
source("map.R")


ui <- fluidPage(
  fluidRow(
      column(
          4, 
          selectInput(
              "selectedState", 
              "State", 
              choices = districtChoices,
              selected = "Utah District 4"
              )
          )
  ),
  fluidRow(
    column(3, plotOutput("stateMap")),
    column(3, plotOutput("districtMap"))
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