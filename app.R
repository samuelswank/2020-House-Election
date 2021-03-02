library(shiny)
library(tidyverse)
source("stringManipulator.R")
source("map.R")
source("flippedModel.R")

stateChoices <- state.name %>% R.utils::insert(1, c(""))
'%!in%' <- function(x,y){!('%in%'(x,y))}

ui <- fluidPage(
  # Dropdown Menu Row
  fluidRow(
    column(
      4, 
      selectInput("selectedState", "State", choices = stateChoices)
      ),
    column(4, uiOutput("selectedDistrict"))
  ),
  # Actual Row
  fluidRow(
    column(4, uiOutput("party"), plotOutput("stateMap")),
    column(4, uiOutput("filler"), plotOutput("districtMap")),
    column(4, uiOutput("flipped"), uiOutput("flippedActual"))
    ),
  # Predicted Row
  fluidRow(
    column(4, plotOutput("predictedState")),
    column(4, plotOutput("predictedDistrict")),
    column(4, uiOutput("flippedPredicted"))
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
  
  # flippedText helper function
  flippedText <- function(resultString) {
    renderUI({
      tags$div(
        style = "margin: 0; position: absolute; top: 300%;",
        tags$b(style = "font-size: 25px", resultString)
      )
    })
  }
  
  
  # Flipped District Model
  observeEvent(input$selectedDistrict, {
    if (
      input$selectedDistrict == "" |
      is.null(reverseDistrict(input$selectedState, input$selectedDistrict)) == TRUE
      ) {
      output$party            <- NULL
      output$filler           <- NULL
      output$flipped          <- NULL
      output$flippedActual    <- NULL
      output$flippedPredicted <- NULL
    } else if (
      reverseDistrict(input$selectedState, input$selectedDistrict) %!in%
      flipped.preds$district
      ) {
      output$party            <- NULL
      output$filler           <- NULL
      output$flipped          <- NULL
      output$flippedActual    <- NULL
      output$flippedPredicted <- NULL
    } else {
      if (flippedResult(input$selectedState, input$selectedDistrict)$in_sample == FALSE) {
        output$flippedAcutal   <- NULL
        output$flippedPreicted <- NULL
      } else {
        output$party <- renderUI({
          tags$div(
            style = "text-align: center", tags$h2("Party Affiliation Model")
            )
          })
        output$filler  <- renderUI({
          h2("----------------------------------------------------")
          })
        output$flipped   <- renderUI({
          tags$div(
            style = "text-align: left", tags$h2("Flipped Model")
          )
        })
        
        # Flipped Actual Text
        if (flippedResult(input$selectedState, input$selectedDistrict)$actual == TRUE) {
          output$flippedActual <- flippedText("FLIPPED REPUBLICAN")
        } else {
          output$flippedActual <- flippedText("STAYED DEMOCRAT")
        }
        
        # Flipped Predicted Text
        if (flippedResult(input$selectedState, input$selectedDistrict)$predicted == TRUE) {
          output$flippedPredicted <- flippedText("FLIPPED REPUBLICAN")
        } else {
          output$flippedPredicted <- flippedText("STAYED DEMOCRAT")
        }
      }
    }
  })
}

shinyApp(ui, server)