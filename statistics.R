library(tidyverse)

# Relevant Statistics

# Walking/Public Transit
# Renter Occupied Units
# Natural-born Citizens
# Commuters by Car
# Owner Occupied Units
# Foreign-born Citizens
# Whites
# Asians
# Persons of Other Races
# Median Rent

modelData <- read_csv("data/census/demographics/preprocessed/modelData.csv")

pieChart <- function(
  selectedState,
  selectedDistrict,
  categories,
  category_strings = NULL,
  fill = NULL,
  n_seed = NULL
  ) {
  
  c <- c()
  for (i in 1:length(categories)) {
    c[i] <- (modelData %>%
      filter(
        districtDemographics == reverseDistrict(
          selectedState, selectedDistrict, atLarge = TRUE
          )
        ) %>%
      select(categories) %>%
      .[[categories[i]]]) / 100
  }

  if (is.null(n_seed) == TRUE) {set.seed(n_seed)}
  
  sampleVec <- sample(categories, 710767, replace = TRUE, prob = c)
  counts <- table(sampleVec) %>% as.data.frame()
  
  if (is.null(category_strings) == TRUE) {
    ggplot(data = counts, aes(x = "", y = Freq, fill = sampleVec)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_discrete(name = "") +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
  } else {
    ggplot(data = counts, aes(x = "", y = Freq, fill = sampleVec)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_discrete(name = "", labels = category_strings) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
  }
}
