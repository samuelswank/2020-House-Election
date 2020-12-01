library(shiny)
library(dotenv)
library(tidycensus)
library(tigris)
library(tidyverse)
source("helpers/houseResults.R")
source("helpers/map.R")
source("helpers/censusTracts.R")

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
    column(6, plotOutput("dualPlot")),
    column(
      3,
      h4("Ethno-Racial Demographics"),
      tableOutput("demographicTable"),
      h4("Sex Ratio"),
      textOutput("sexRatio")
      ),
    column(3, h4("Voting Age Population Statistics"), tableOutput("vaTable"))
  )
)

server <- function(input, output, session) {
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
    
    output$dualPlot <- renderPlot({
      dualPlot(
        plotState(
          stateData(input$selectedDistrict, stateString(input$selectedDistrict)),
          input$selectedDistrict
        ),
        plotDistrict(
          stateData(input$selectedDistrict, stateString(input$selectedDistrict)),
          input$selectedDistrict
        )
      )
    })
    
    output$demographicTable <- renderTable(
      rownames = TRUE, digits = 0, bordered = TRUE, hover = TRUE, na = "",
      {
      demographicTable(
        input$selectedDistrict, demographicData(input$selectedDistrict)
        )
      }
    )
    
    output$sexRatio <- renderText({
      sexRatio(input$selectedDistrict, demographicData(input$selectedDistrict))
      })
    
    output$vaTable <- renderTable(
      rownames = TRUE, digits = 0, bordered = TRUE, hover = TRUE, na = "",
      {vaTable(input$selectedDistrict)}
      )
}

shinyApp(ui, server)