library(shiny)
library(leaflet)

centerText <- function(htmlText) {
  return(tags$div(style = "text-align: center;", htmlText))
}

eeText <- tags$body(
  tags$div(
    style = "border: 1px solid black; margin: 25px 200px 25px 200px;
    background-color: #2FABE1; font-size: large; color: black; font-weight: 900;",
    centerText(h1("A Note From the Creator")),
    tags$p("This web applicaiton was originally intended to explore what
    characteristics made the House districts which flipped from Democrat to
    Republican unique. Due to time constraints and statistical challenges, this
    analysis has since been perforemed in a non-predictive manner and moved to a
           notebook on RPubs (TODO - URL)."),
    
    tags$p("During the preliminary research into each of the flipped districts,
    however, I discovered that Oklahoma's 5th Congressional District Seat, was
    once filled by my paternal great-grandfather's cousin, one Fletcher B.
           Swank.")
  )
)

fpList1 <- list(
  style = "display: block;
          margin-left: auto;
          margin-right: auto;
          width: 303px;
  height: 412px;",
  src = paste("data", "images", "11889226_120624232039.jpg", sep = "/"),
  filetype = "image/jpeg"
)

fpList2 <- list(
  style = fpList1$style,
  src = paste("data", "images", "FletcherBSwank.jpg", sep = "/"),
  filetype = fpList1$filetype
)

oldOK <- rgdal::readOGR(paste("data", "Oklahoma_64_to_72.geojson", sep = "/"))
oldMap <- leaflet(oldOK) %>% addTiles() %>% addPolygons(label = oldOK$district)

