library(tidyverse)
library(sf)
source("partyModel.R")
source("dataWrangling/houseResults.R")

# Congressional District sf dataframe
cd117 <- tigris::congressional_districts(year = 2019)
# Calculating centroids for congressional district geometries
cd117 <- cbind(cd117, st_coordinates(st_centroid(cd117$geometry)))

# Party Affiliation Predictions
partyPreds <- pa.preds

# Addressing  "(At Large)" Districts
partyPreds$district <- partyPreds$district %>%
  sapply(function(x) gsub( " *\\(.*?\\) *", " 1", x))



# Combines House Results, Party Affiliation Predictions, and each district's
# appropriate shape files
stateData <- function(selectedState) {
  stateParty <- hr %>%
    filter(fipsCode == fipsList[[selectedState]]$st) %>%
    merge(partyPreds, by = c("state", "district")) %>%
    select(4, 2, 6, 11, 13)
  
  if (selectedState %in% atLarge) {
    stateParty$district[1] <- "Congressional District (at Large)"
  }
  
  # STATEFP  - State Fips Code
  colnames(stateParty)[1] <- "STATEFP"
  # NAMELSAD - Congressional District
  colnames(stateParty)[2] <- "NAMELSAD"

  shapeFile <- cd117  %>%
    filter(STATEFP == fipsList[[selectedState]]$st) %>%
    merge(stateParty, by = c("STATEFP", "NAMELSAD"))
  
  return(shapeFile)
}

# Subsets single district out of state sf dataframe subset
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
      ylab("Actual") +
      theme(
        panel.background = element_blank(),
        axis.title.y.left = element_text(
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
      ylab("Actual") +
      theme(
        panel.background = element_blank(),
        axis.title.y.left = element_text(
          hjust = 0.5, size = 24, family = "NewCenturySchoolbook"
        ),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
      )
    }
}

plotDistrict <- function(selectedState, selectedDistrict) {
  if (districtData(selectedState, selectedDistrict)$flipped[1] == FALSE) {
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
  } else {
    ggplot() + 
      geom_sf(
        districtData(selectedState, selectedDistrict),
        mapping = aes(fill = party), 
        size = 0.75, 
        color = "black"
      ) +
      scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
      labs(title = selectedDistrict) +
      geom_text(districtData(selectedState, selectedDistrict), mapping = aes(X, Y, label = flipped)) +
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
}

plotPredicted <- function(selectedState, selectedDistrict, geography) {
  if (is.na(selectedDistrict) == TRUE & geography == "state") {
    if (selectedState == "Alaska") {
      ggplot() + 
        geom_sf(
          stateData(selectedState),
          mapping = aes(fill = predicted),
          size = 0.75,
          color = "black"
        ) +
        xlim(-180, -120) +
        scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
        ylab("Predicted") +
        theme(
          panel.background = element_blank(),
          axis.title.y.left = element_text(
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
          mapping = aes(fill = predicted),
          size = 0.75,
          color = "black"
        ) +
        scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
        ylab("Predicted") +
        theme(
          panel.background = element_blank(),
          axis.title.y.left = element_text(
            hjust = 0.5, size = 24, family = "NewCenturySchoolbook"
          ),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none"
        )
    }
  } else if (geography == "district") {
    ggplot() + 
      geom_sf(
        districtData(selectedState, selectedDistrict),
        mapping = aes(fill = predicted), 
        size = 0.75, 
        color = "black"
      ) +
      scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
      theme(
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
      )
  }
}
