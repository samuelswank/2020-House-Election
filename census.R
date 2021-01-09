library(tidyverse)

df <- read_csv("data/census/demographics/preprocessed/districtDemographics.csv")

districtChoices <- list()
for (stadt in state.name) {districtChoices[[stadt]] <- c(NA)}

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
  districtChoices[[i]] <- districtChoices[[i]][2:length(districtChoices[[i]])]
  districtChoices[[i]] <- gtools::mixedsort(districtChoices[[i]])
}


