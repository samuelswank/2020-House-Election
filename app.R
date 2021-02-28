library(shiny)
library(tidyverse)
source("stringManipulator.R")
source("map.R")
source("flippedModel.R")

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
    column(4, plotOutput("stateMap")),
    column(4, plotOutput("districtMap")),
    column(4, uiOutput("flipped"), uiOutput("flippedActual"))
    ),
  fluidRow(
    column(4, plotOutput("predictedState")),
    column(4, plotOutput("predictedDistrict")),
    column(4, textOutput("flippedPredicted"), uiOutput("flippedPredicted"))
    )
)


server <- function(input, output, session) {
  output$selectedDistrict <- renderUI({
    selectInput(
      "selectedDistrict", "District", districtChoices[[input$selectedState]]
      )
  })
  
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
  
  observeEvent(input$selectedDistrict, {
    if (
      input$selectedDistrict == "" |
      input$selectedDistrict == "Congressional District (At Large)" |
      is.null(reverseDistrict(input$selectedState, input$selectedDistrict)) == TRUE
      ) {
      output$flippedActual    <- NULL
      output$flippedPredicted <- NULL
    } else if (
      reverseDistrict(input$selectedState, input$selectedDistrict) %!in%
      flipped.preds$district
      ) {
      output$flippedActual    <- NULL
      output$flippedPredicted <- NULL
    } else {
      output$flipped <- renderUI({h1("Flipped Republican?")})
      if (flippedResult(input$selectedState, input$selectedDistrict)$in_sample == FALSE) {
        output$flippedAcutal   <- NULL
        output$flippedPreicted <- NULL
      } else {
        if (flippedResult(input$selectedState, input$selectedDistrict)$actual == TRUE) {
          output$flippedActual <- renderUI({tags$b("FLIPPED")})
        } else {output$flippedActual <- renderUI({tags$b("NOT FLIPPED")})}

        if (flippedResult(input$selectedState, input$selectedDistrict)$predicted == TRUE) {
          output$flippedPredicted <- renderUI({tags$b("FLIPPED")})
        } else {output$flippedPredicted <- renderUI({tags$b("NOT FLIPPED")})}
      }
    }
  })
}

shinyApp(ui, server)