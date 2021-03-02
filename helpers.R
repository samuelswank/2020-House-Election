library(shiny)

centerText <- function(htmlText) {
  return(tags$div(style = "text-align: center;", htmlText))
  }