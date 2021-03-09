library(shiny)

centerText <- function(htmlText) {
  return(tags$div(style = "text-align: center;", htmlText))
}

eeTitle <- centerText(h1("A Note From the Creator"))

eeText <- tags$body(
  tags$div(
    style = "border: 1px solid black; margin: 25px 200px 25px 200px;
    background-color: #1B4E81; font-size: large; color: black; font-weight: 900;",
    tags$p("This web applicaiton was originally intended to explore what
    characteristics made the House districts which flipped from Democrat to
    Republican unique. Due to certain statistical challenges involved with
    training a machine learning model to accurately predict whether or not a
    previously-Democratic district would flip Republican, this analysis has
    since been perforemed in a non - predictive manner and moved to a notebook
           on RPubs (TODO - URL)."),
    
    tags$p("During the preliminary research into each of the flipped districts,
    however, I discovered that Oklahoma's 5th Congressional District, was once
    filled by my paternal great-grandfather's cousin, one
           Fletcher B. Swank.")
  )
)