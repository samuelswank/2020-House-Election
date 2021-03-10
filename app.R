library(shiny)
library(tidyverse)
library(gridExtra)
source("stringManipulator.R")
source("map.R")
source("statistics.R")
source("easterEgg.R")

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
    column(
      4,
      uiOutput("importancesTitle"),
      tableOutput("importancesTable"),
      uiOutput("rentTitle"),
      uiOutput("medRent")
      ),
    column(4, uiOutput("statTitle"), plotOutput("statPlot")),
    column(2)
    ),
  fluidRow(column(12, h1(""))),
  fluidRow(column(12, h1(""))),
  fluidRow(column(12, h1(""))),
  # fluidRow(column(12), uiOutput("eeTitle")),
  fluidRow(column(12), uiOutput("eeText")),
  fluidRow(column(12), uiOutput("fTitle")),
  fluidRow(
    column(3),
    column(3, imageOutput("fp")),
    column(3, imageOutput("fp2")),
    column(3)
    )
)


server <- function(input, output, session) {
  output$selectedDistrict <- renderUI({
    selectInput(
      "selectedDistrict", "District", districtChoices[[input$selectedState]]
      )
  })
  
  toListen <- reactive({list(input$selectedState,input$selectedDistrict)})
  
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
      output$rentTitle        <- NULL
      output$medRent          <- NULL
    } else if (input$selectedDistrict != "") {
      output$statistics <- renderUI({statistics})
      output$statInput <- renderUI({
        selectInput(
          "selectedChart", "Chart", choices = c(
            "",
            "Race",
            "Native-born and Naturalized Citizens",
            "Commuter Method",
            "Residential Occupancy",
            "Rental Data"
            ),
          selected = "Commuter Method"
          )
      })
      output$importancesTitle <- renderUI({h3("Importances")})
      output$importancesTable <- renderTable(topTen, rownames = TRUE)
      output$rentTitle <- renderUI({h3("Median Rent")})
      output$medRent <- renderUI({
        h4(
          paste(
            "$",
            getRent(input$selectedState, input$selectedDistrict),
            " per month",
            sep = ""
            )
          )
        })
      
      observeEvent(input$selectedChart, {
        if (is.null(input$selectedChart) == TRUE) {
          output$statPlot   <- NULL
          output$statTitle  <- NULL
        } else if (input$selectedChart == "") {
          output$statPlot <- NULL
          output$statTitle  <- NULL
        } else if (input$selectedChart == "Race") {
          output$statTitle <- renderUI({h3(input$selectedChart)})
          output$statPlot <- renderPlot(width = 575, height = 475, expr = {
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
          output$statTitle <- renderUI({h3(input$selectedChart)})
          output$statPlot <- renderPlot(width = 575, height = 475, expr = {
            barChart(
              input$selectedState,
              input$selectedDistrict,
              categories = colnames(modelData)[19:23],
              category_strings = c(
                "Born Abroad or in US Territory",
                "Born in State",
                "Born Out of State",
                "Foreign-born",
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
        } else if (input$selectedChart == "Commuter Method") {
          output$statTitle <- renderUI({h3(input$selectedChart)})
          output$statPlot <- renderPlot(width = 575, height = 475, {
            barChart(
              input$selectedState,
              input$selectedDistrict,
              categories = colnames(modelData)[32:33],
              category_strings = c(
                "By Car (including carpooling)", "Walking / Public Transit"
                ),
              n_seed = 42
            )
          })
        } else if (input$selectedChart == "Residential Occupancy") {
          output$statTitle <- renderUI({h3(input$selectedChart)})
          
          occupancyPlot <- barChart(
            input$selectedState,
            input$selectedDistrict,
            categories = colnames(modelData)[41:42],
            category_strings = c(
              "Owner Occupied", "Rental Occupied"
            ),
            n_seed = 42
          )
          
          vacancyPlot <- barChart(
            input$selectedState,
            input$selectedDistrict,
            categories = colnames(modelData)[38:39],
            category_strings = c(
              "Homeowner vacancy", "Rental vacancy"
            ),
            n_seed = 42
          )
          
          output$statPlot <- renderPlot(width = 575, height = 475, {
            grid.arrange(occupancyPlot, vacancyPlot, ncol = 2)
          })
        } else if (input$selectedChart == "Rental Data") {
          output$statTitle <- renderUI({h3(input$selectedChart)})
          
          output$statPlot <- renderPlot( width = 575, height = 475, {
            densityPlot(input$selectedState, input$selectedDistrict)
          })
        }
      })
    }
  })
  
  observeEvent(toListen(), {
    if (is.null(input$selectedDistrict) == TRUE) {
      # output$eeTitle <- NULL
      output$eeText  <- NULL
      output$fTitle  <- NULL
      output$fp      <- NULL
      output$fp2     <- NULL
    } else if (
      input$selectedState != "Oklahoma" |
      input$selectedDistrict != "Congressional District 5"
    ) {
      # output$eeTitle <- NULL
      output$eeText  <- NULL
      output$fTitle  <- NULL
      output$fp      <- NULL
      output$fp2     <- NULL
    } else if (
      input$selectedState == "Oklahoma" &
      input$selectedDistrict == "Congressional District 5"
    ) {
      output$eeTitle <- renderUI({eeTitle})
      output$eeText <- renderUI({eeText})
      output$fTitle <- renderUI({
        centeredText(h2("Fletcher B. Swank"))
        })
      output$fp <- renderImage({fpList1})
      output$fp2 <- renderImage({fpList2})
    }
  })
}

shinyApp(ui, server)