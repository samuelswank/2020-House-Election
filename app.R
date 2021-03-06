library(shiny)
library(tidyverse)
source("helpers.R")
source("stringManipulator.R")
source("map.R")
source("statistics.R")

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
  fluidRow(column(5), column(2, uiOutput("statInput")), column(3)),
  fluidRow(
    column(2),
    column(4, uiOutput("importancesTitle"), tableOutput("importancesTable")),
    column(4, uiOutput("statTitle"), plotOutput("statPlot"))
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
      output$stateMap          <- NULL
      output$districtMap       <- NULL
      output$predictedState    <- NULL
      output$predictedDistrict <- NULL
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
      output$districtMap <- renderPlot({plotState(input$selectedState)})
      output$predictedState <- renderPlot({
        plotPredicted(input$selectedState, NA, geography = "state")
      })
      output$predictedDistrict <- renderPlot({
        plotPredicted(input$selectedState, NA, geography = "state")
      })
    }
  })
  
  observeEvent(input$selectedDistrict, {
    if (input$selectedDistrict == "") {
      output$statistics       <- NULL
      output$statInput        <- NULL
      output$importancesTitle <- NULL
      output$importancesTable <- NULL
      output$statTitle        <- NULL
      output$statPlot         <- NULL
    } else if (input$selectedDistrict != "") {
      output$statistics <- renderUI({statistics})
      output$statInput <- renderUI({
        selectInput(
          "selectedChart", "Chart", choices = c(
            "",
            "Race",
            "Native-born and Naturalized Citizens",
            "Commuter Method",
            "Residential Data"
            ),
          selected = ""
          )
      })
      output$importancesTitle <- renderUI({h3("Importances")})
      output$importancesTable <- renderTable(topTen, rownames = TRUE)
      
      observeEvent(input$selectedChart, {
        if (is.null(input$selectedChart) == TRUE) {
          output$statPlot <- NULL
          output$statTitle  <- NULL
        } else if (input$selectedChart == "") {
          output$statPlot <- NULL
          output$statTitle  <- NULL
        } else if (input$selectedChart == "Race") {
          output$statTitle <- renderUI({centerText(h3(input$selectedChart))})
          output$statPlot <- renderPlot(width = 425, height = 575, expr = {
            pieChart(
              input$selectedState,
              input$selectedDistrict,
              colnames(modelData)[7:13],
              category_strings = c(
                "American Indians",
                "Asians",
                "Blacks",
                "Pacific Islanders",
                "Multiracial Persons",
                "Persons of Other Races",
                "White"
              ),
              n_seed = 42
            )
          })
        } else if (input$selectedChart == "Native-born and Naturalized Citizens") {
          output$statTitle <- renderUI({centerText(h3(input$selectedChart))})
          output$statPlot <- renderPlot(width = 425, height = 575, expr = {
            barChart(
              input$selectedState,
              input$selectedDistrict,
              categories = colnames(modelData)[19:23],
              category_strings = c(
                "Born Abroad or in US Territory",
                "Born in State",
                "Born Out of State",
                "Foreign-born (Naturalized)",
                "Natural Born"
                ),
              grouping = c(
                "Natural-born",
                "Natural-born",
                "Natural-born",
                "Naturalized",
                "Natural-born"
                ),
              n_seed = 42
            )
          })
        }
      })
    }
  })
}

shinyApp(ui, server)