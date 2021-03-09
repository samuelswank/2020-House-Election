library(shiny)

centerText <- function(htmlText) {
  return(tags$div(style = "text-align: center;", htmlText))
}

eeTitle <- centerText("A Note From the Creator")

eeText <- tags$body(
  tags$p("This web applicaiton was originally intended to explore what
         characteristics made the House districts which flipped from Democrat
         to Republican Unique.")
)