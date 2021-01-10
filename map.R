library(tidyverse)
source("dataWrangling/houseResults.R")

cd117 <- tigris::congressional_districts(year = 2019)

stateData <- function(selectedState) {
  stateParty <- hr %>%
    filter(fipsCode == fipsList[[selectedState]]$st) %>%
    select(c(3, 4, 6, 11))
  
  if (selectedState %in% atLarge) {
    stateParty$district[1] <- "Congressional District (at Large)"
  }

  colnames(stateParty)[1] <- "STATEFP"
  colnames(stateParty)[2] <- "NAMELSAD"

  shapeFile <- cd117  %>%
    filter(STATEFP == fipsList[[selectedState]]$st) %>%
    merge(stateParty, by = c("STATEFP", "NAMELSAD"))
  
  return(shapeFile)
}

districtData <- function(selectedState, selectedDistrict) {
  return(stateData(selectedState) %>% filter(NAMELSAD == selectedDistrict))
}

plotState <- function(selectedState) {
  if (selectedState == "Alaska") {
    ggplot() + 
      geom_sf(
        stateData(selectedState),
        mapping = aes(fill = party),
        size = 0.75,
        color = "black"
      ) +
      xlim(-180, -120) +
      scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
      ggtitle(selectedState) +
      theme(
        panel.background = element_blank(),
        plot.title = element_text(
          hjust = 0.5, size = 24, family = "NewCenturySchoolbook"
        ),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
      )
  } else {
    ggplot() + 
      geom_sf(
        stateData(selectedState),
        mapping = aes(fill = party),
        size = 0.75,
        color = "black"
      ) +
      scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
      ggtitle(selectedState) +
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
}

plotDistrict <- function(selectedState, selectedDistrict) {
  ggplot() + 
    geom_sf(
      districtData(selectedState, selectedDistrict),
      mapping = aes(fill = party), 
      size = 0.75, 
      color = "black"
      ) +
    scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
    labs(title = selectedDistrict) +
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
