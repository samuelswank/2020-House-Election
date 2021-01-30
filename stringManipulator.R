library(tidyverse)

# Addressing single district states

substrRight <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}

atLarge <- character()

for (f in list.files("data/census/demographics/raw/")) {
  if (substrRight(f, 9) == "Large.csv") {
    # Accounting for North and South Dakota
    if (
      strsplit(f, split = "_")[[1]][1] == "North" |
      strsplit(f, split = "_")[[1]][1] == "South"
    ) {
      atLarge <- atLarge %>% append(
        paste(
          strsplit(f, split = "_")[[1]][1], strsplit(f, split = "_")[[1]][2]
        )
      )
    } else {atLarge <- atLarge %>% append(strsplit(f, split = "_")[[1]][1])}
  }
}

# Creating dynamic choice list by state

df <- read_csv("data/census/demographics/preprocessed/modelData.csv")

districtChoices <- list()
for (stadt in state.name) {districtChoices[[stadt]] <- c("")}

for (i in 1:length(df$districtDemographics)) {
  if (strsplit(df$districtDemographics[i], split = " ")[[1]][1] %in% atLarge) {
    
    districtChoices[[strsplit(df$districtDemographics[i], split = " ")[[1]][1]]] <-
      districtChoices[[strsplit(df$districtDemographics[i], split = " ")[[1]][1]]] %>%
      append("Congressional District (At Large)")
    
  } else if (
    paste(
      strsplit(df$districtDemographics[i], split = " ")[[1]][1],
      strsplit(df$districtDemographics[i], split = " ")[[1]][2]
    ) %in% atLarge
  ) {
    districtChoices[[
      paste(
        strsplit(df$districtDemographics[i], split = " ")[[1]][1],
        strsplit(df$districtDemographics[i], split = " ")[[1]][2]
      ) 
    ]] <- districtChoices[[
      paste(
        strsplit(df$districtDemographics[i], split = " ")[[1]][1],
        strsplit(df$districtDemographics[i], split = " ")[[1]][2]
      )
    ]] %>% 
      append("Congressional District (At Large)")
  } else if (
    strsplit(df$districtDemographics[i], split = " ")[[1]][1] %in% state.name
  ) {
    districtChoices[[strsplit(df$districtDemographics[i], split = " ")[[1]][1]]] <-
      districtChoices[[strsplit(df$districtDemographics[i], split = " ")[[1]][1]]] %>%
      append(
        paste(
          "Congressional District",
          tail(strsplit(df$districtDemographics[i], split = " ")[[1]], 1),
          sep = " "
        )
      )
  } else if (
    paste(
      strsplit(df$districtDemographics[i], split = " ")[[1]][1],
      strsplit(df$districtDemographics[i], split = " ")[[1]][2]
    ) %in% state.name
  ) {
    districtChoices[[
      paste(
        strsplit(df$districtDemographics[i], split = " ")[[1]][1],
        strsplit(df$districtDemographics[i], split = " ")[[1]][2]
      ) 
    ]] <- districtChoices[[
      paste(
        strsplit(df$districtDemographics[i], split = " ")[[1]][1],
        strsplit(df$districtDemographics[i], split = " ")[[1]][2]
      )
    ]] %>% 
      append(
        paste(
          "Congressional District",
          tail(strsplit(df$districtDemographics[i], split = " ")[[1]], 1),
          sep = " "
        )
      )
  }
}

for (i in 1:length(districtChoices)) {
  districtChoices[[i]] <- gtools::mixedsort(districtChoices[[i]])
}

# for (stadt in state.name) {
#   if (stadt %in% atLarge) {
#     districtChoices[[stadt]] <- districtChoices[[stadt]][2]
#   }
# }



