library(shiny)
library(tidyverse)
library(gridExtra)
library(leaflet)
source("helpers/stringManipulator.R")
source("helpers/plotting/map.R")
source("helpers/plotting/statistics.R")
source("helpers/plotting/easterEgg.R")

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
  # Statistical Tables and Charts
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
  
  # Oklahoma 5th District Easter Egg: 
  # Short Biography of Fletcher B. Swank
  fluidRow(column(12), uiOutput("eeText")),
  fluidRow(
    column(2),
    column(3, uiOutput("fbs"), uiOutput("wcs")),
    column(5, uiOutput("eeText2"), uiOutput("eeText3")),
    column(2)
    ),
  fluidRow(column(12, h1(""))),
  
  # Old Oklahoma District Map
  fluidRow(
    column(3),
    column(
      6,
      uiOutput("old_map_title"),
      leafletOutput("oldMap")
      ),
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
      output$stateMap          <- renderPlot({plotState(input$selectedState)})
      output$districtMap       <- renderPlot({
        plotDistrict(input$selectedState, input$selectedDistrict)
        })
      output$predictedState    <- renderPlot({
        plotPredicted(input$selectedState, NA, geography = "state")
        })
      output$predictedDistrict <- renderPlot({
        plotPredicted(
          input$selectedState, input$selectedDistrict, geography = "district"
          )
      })
      
    } else if (input$selectedState %in% atLarge) {
      output$stateMap          <- renderPlot({plotState(input$selectedState)})
      output$districtMap       <- renderPlot({plotState(input$selectedState)})
      output$predictedState    <- renderPlot({
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
      output$statInput  <- renderUI({
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
      output$rentTitle        <- renderUI({h3("Median Rent")})
      output$medRent          <- renderUI({
        h4(
          paste(
            "$",
            getRent(input$selectedState, input$selectedDistrict),
            " per month",
            sep = ""
            )
          )
        })
      
      # Statistical Charts
      observeEvent(input$selectedChart, {
        if (is.null(input$selectedChart) == TRUE) {
          output$statPlot   <- NULL
          output$statTitle  <- NULL
        } else if (input$selectedChart == "") {
          output$statPlot   <- NULL
          output$statTitle  <- NULL
          
        # Race
        } else if (input$selectedChart == "Race") {
          output$statTitle <- renderUI({h3(input$selectedChart)})
          output$statPlot  <- renderPlot(width = 575, height = 475, expr = {
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
        
        # Birthplace
        } else if (input$selectedChart == "Native-born and Naturalized Citizens") {
          output$statTitle <- renderUI({h3(input$selectedChart)})
          output$statPlot  <- renderPlot(width = 575, height = 475, expr = {
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
          
        # Commute
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
          
        # Housing Occupancy
        } else if (input$selectedChart == "Residential Occupancy") {
          output$statTitle <- renderUI({h3(input$selectedChart)})
          
          output$statPlot <- renderPlot(width = 575, height = 475, {
            barChart(
              input$selectedState,
              input$selectedDistrict,
              categories = colnames(modelData)[41:42],
              category_strings = c(
                "Owner Occupied", "Rental Occupied"
              ),
              n_seed = 42
            )
          }) 
          
        # Median Rental Price Distribution
        } else if (input$selectedChart == "Rental Data") {
          output$statTitle <- renderUI({h3(input$selectedChart)})
          
          output$statPlot <- renderPlot( width = 575, height = 475, {
            densityPlot(input$selectedState, input$selectedDistrict)
          })
        }
      })
    }
  })
  
  # Oklahoma 5th District Easter Egg: 
  # Short Biography of Fletcher B. Swank
  observeEvent(toListen(), {
    if (is.null(input$selectedDistrict) == TRUE) {
      output$eeText        <- NULL
      output$fbs           <- NULL
      output$wcs           <- NULL
      output$eeText2       <- NULL
      output$eeText3       <- NULL
      output$old_map_title <- NULL
      output$oldMap        <- NULL
    } else if (
      input$selectedState != "Oklahoma" |
      input$selectedDistrict != "Congressional District 5"
    ) {
      output$eeText        <- NULL
      output$fbs           <- NULL
      output$wcs           <- NULL
      output$eeText2       <- NULL
      output$eeText3       <- NULL
      output$old_map_title <- NULL
      output$oldMap        <- NULL
    } else if (
      input$selectedState == "Oklahoma" &
      input$selectedDistrict == "Congressional District 5"
    ) {
      output$eeText  <- renderUI({eeText})
      output$fbs     <- renderUI({
        tags$figure(
          renderImage(deleteFile = FALSE, {
            list(
              style = "display: block; margin-left: auto; margin-right: auto;",
              src = "data/images/FletcherBSwank.jpg",
              filetype = "image/jpeg",
              height = "400",
              width = "300"
            )
          }),
          tags$figcaption(
            style = "text-align: center;",
            tags$b("Representative Fletcher B. Swank")
            )
        )
      })

      output$wcs     <- renderUI({
        tags$figure(
          renderImage(deleteFile = FALSE, {
            list(
              style = "display:block;
              margin-right: auto;
              margin-left: auto;
              margin-bottom: 0;",
              src = "data/images/WilliamClaySwankI.jpg",
              filetype = "image/jpeg",
              height = "400",
              width = "400"
            )
          }),
          tags$figcaption(
            style = "text-align: center;",
            tags$b("My Great-Grandfather, William C. Swank I")
          )
        )
      })
      
      output$eeText2 <- renderUI({eeText2})
      output$eeText3 <- renderUI({eeText3})
      output$old_map_title <- renderUI({
        centerText(h3("Oklahoma Congressional Districts, 1914 - 1940"))
        }) 
      
      output$oldMap        <- renderLeaflet({oldMap})
    }
  })
}

shinyApp(ui, server)