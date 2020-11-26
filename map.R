library(tigris)
library(tidyverse)
source("houseResults.R")
source("stringManipulator.R")

cd117 <- congressional_districts(year = 2019)

stateData <- function(selectedDistrict, stateString) {
  stateName <- stateString(selectedDistrict)
  stateParty <- flippedResults %>% filter(fipsCode == fipsList[[stateName]]$st)

  colnames(stateParty)[3] <- "STATEFP"
  colnames(stateParty)[4] <- "NAMELSAD"

  shapeFile <- cd117  %>%
    filter(STATEFP == fipsList[[stateName]]$st) %>%
    merge(stateParty, by = c("STATEFP", "NAMELSAD"))
  
  return(shapeFile)
}

districtData <- function(stateData, selectedDistrict) {
  return(
    stateData %>% 
      subset(str_detect(NAMELSAD, districtString(selectedDistrict)) == TRUE)
    )
}

plotState <- function(stateData, selectedDistrict) {
  ggplot() + 
    geom_sf(
      stateData, mapping = aes(fill = party), size = 0.75, color = "black"
      ) +
    scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
    ggtitle(stateString(selectedDistrict)) +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(
        hjust = 0.5, size = 24, family = "NewCenturySchoolbook"
        ),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
}

plotDistrict <- function(stateData, selectedDistrict) {
  ggplot() + 
    geom_sf(
      districtData(stateData, selectedDistrict),
      mapping = aes(fill = party), 
      size = 0.75, 
      color = "black"
      ) +
    scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
    labs(title = paste("District", districtString(selectedDistrict))) +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(
        hjust = 0.5, size = 18, family = "NewCenturySchoolbook"
        ),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
      )
}
