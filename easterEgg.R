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
    however, I discovered that Oklahoma's 5th Congressional District seat, was
    once filled by my paternal great-grandfather's cousin, one Fletcher B.
           Swank.")
  )
)

eeText2 <- tags$body(
  tags$div(
    style = "border: 1px solid black; margin-top: 12.5%; margin-bottom: 50;
    background-color: #2FABE1; font-size: large; color: black; font-weight: 900;",
    tags$p("Fletcher B. Swank was born in Davis County, Iowa, west of the Des
           Moines River, near the border with Missouri in 1875. As a child,
           Fletcher moved with his parents, Wallace and Melinda Wells Swank, to
           Indian Territory, now Oklahoma. In Oklahoma, Fletcher would make his
           career as a school superintendent, lawyer, and congressman."),
    tags$p("Fletcher served as a member of the House of Representatives from
           1921 - 1929 and from 1931 - 1935. He was a Democrat.")
  )
)

eeText3 <- tags$body(
  tags$div(
    style = "border: 1px solid black; margin-top: 33.5%;
    background-color: #2FABE1; font-size: large; color: black; font-weight: 900;",
    tags$p("My great-grandfather, William Clay Swank I, was born in neighboring
           Appanouse County in 1880 to Cyrus and Mary Swank. After working in
           the railroad industry in New Mexico for a number of years, he settled
           in the great state of Idaho where he worked as a bookkeeper. It was
           in Pocatello, Idaho that my grandfather, George Wallace Swank was
           born. My father, William Clay Swank II, a native of Boise, was named
           after his grandfather.")
  )
)

oldOK <- rgdal::readOGR(paste("data", "Oklahoma_64_to_72.geojson", sep = "/"))
oldMap <- leaflet(oldOK) %>% addTiles() %>% addPolygons(label = oldOK$district)

