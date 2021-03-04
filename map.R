library(tidyverse)
library(sf)
library(ggrepel)
source("partyModel.R")
source("dataWrangling/houseResults.R")

'%!in%' <- function(x,y){!('%in%'(x,y))}

# Congressional District sf dataframe
cd117 <- tigris::congressional_districts(year = 2019)
# Calculating centroids for congressional district geometries
cd117 <- cbind(cd117, st_coordinates(st_centroid(cd117$geometry)))

# Calculating area of each district polygon.
area <- c()
for (i in 1:nrow(cd117)) {area[i] <- st_area(cd117$geometry[[i]])}

cd117$area <- area

# House Results dataframe
# Fixing flipped column so that it conveys information better
flipped <- c()
for (i in 1:nrow(hr)) {
  if (hr$party[i] == "R" & hr$flipped[i] == TRUE) {
    flipped[i] <- "Flipped Republican"
  } else if (hr$party[i] == "D" & hr$flipped[i] == TRUE) {
    flipped[i] <- "Flipped Democrat"
  } else {
    flipped[i] <- "FALSE"
  }
}

hr$flipped <- flipped

# For ease in labeling districts on map
districtLabel <- c()
districtSplit <- strsplit(hr$district, split = " ")

for (i in 1:length(districtSplit)) {
  districtLabel[i] <- districtSplit[[i]][3]
}

hr$district_label <- districtLabel

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
    select(4, 2, 6, 11, 12, 14)
  
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

titleFormat <- element_text(
  hjust = 0.5, size = 24, family = "NewCenturySchoolbook"
)

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
      ylab("Actual") +
      theme(
        panel.background = element_blank(),
        plot.title = titleFormat,
        axis.title.y.left = titleFormat,
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
      )
  } else if (selectedState %in% atLarge | selectedState == "Hawaii") {
    ggplot() + 
      geom_sf(
        stateData(selectedState),
        mapping = aes(fill = party),
        size = 0.75,
        color = "black"
      ) +
      scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
      ggtitle(selectedState) +
      ylab("Actual") +
      theme(
        panel.background = element_blank(),
        plot.title = titleFormat,
        axis.title.y.left = titleFormat,
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
      geom_text_repel(
        stateData(selectedState),
        mapping = aes(X, Y, label = district_label), 
        fontface = "bold",
        size = 5,
        segment.linetype = 1,
        max.overlaps = 10
        ) +
      scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
      ggtitle(selectedState) +
      ylab("Actual") +
      theme(
        panel.background = element_blank(),
        plot.title = titleFormat,
        axis.title.y.left = titleFormat,
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
        )
    }
}

plotDistrict <- function(selectedState, selectedDistrict) {
  if (selectedDistrict == "") {return(NULL)}
  else if (
    districtData(selectedState, selectedDistrict)$flipped[1] == "Flipped Republican" |
    districtData(selectedState, selectedDistrict)$flipped[1] == "Flipped Democrat"
    ) {
    if (selectedState != "Minnesota" & selectedState != "Utah") {
      ggplot() + 
        geom_sf(
          districtData(selectedState, selectedDistrict),
          mapping = aes(fill = party), 
          size = 0.75, 
          color = "black"
        ) +
        geom_text_repel(
          districtData(selectedState, selectedDistrict),
          mapping = aes(X, Y, label = flipped), 
          fontface = "bold",
          size = 10,
          segment.linetype = 1,
          max.overlaps = 10
        ) +
        scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
        labs(title = selectedDistrict) +
        theme(
          panel.background = element_blank(),
          plot.title = element_text(
            hjust = 0.5, size = 18, family = "NewCenturySchoolbook"
          ),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
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
        geom_text_repel(
          districtData(selectedState, selectedDistrict),
          mapping = aes(X, Y, label = flipped), 
          fontface = "bold",
          size = 5,
          segment.linetype = 1,
          max.overlaps = 10
        ) +
        scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
        labs(title = selectedDistrict) +
        theme(
          panel.background = element_blank(),
          plot.title = element_text(
            hjust = 0.5, size = 18, family = "NewCenturySchoolbook"
          ),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none"
        )
    }
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
        ggtitle(selectedState) +
        ylab("Predicted") +
        theme(
          panel.background = element_blank(),
          plot.title = titleFormat,
          axis.title.y.left = titleFormat,
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none"
        )
    } else if (selectedState %in% atLarge | selectedState == "Hawaii") {
      ggplot() + 
        geom_sf(
          stateData(selectedState),
          mapping = aes(fill = predicted),
          size = 0.75,
          color = "black"
        ) +
        scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
        ggtitle(selectedState) +
        ylab("Predicted") +
        theme(
          panel.background = element_blank(),
          plot.title = titleFormat,
          axis.title.y.left = titleFormat,
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
        geom_text_repel(
          stateData(selectedState),
          mapping = aes(X, Y, label = district_label), 
          fontface = "bold",
          size = 5,
          segment.linetype = 1,
          max.overlaps = 10
          ) +
        scale_fill_manual(values = c("R" = "#D20F26", "D" = "#1B4E81")) +
        ylab("Predicted") +
        theme(
          panel.background = element_blank(),
          plot.title = titleFormat,
          axis.title.y.left = titleFormat,
          axis.text = element_blank(),
          axis.title.x = element_blank(),
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
