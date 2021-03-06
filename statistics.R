library(tidyverse)
library(scales)

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

'%!in%' <- function(x,y){!('%in%'(x,y))}

modelData <- read_csv("data/census/demographics/preprocessed/modelData.csv")

getRent <- function(selectedState, selectedDistrict) {
  if (selectedState %!in% atLarge) {
    districtString <- reverseDistrict(selectedState, selectedDistrict)
  } else {districtString <- paste(selectedState, "At Large", sep = " ")}
  
  med_rent <- modelData %>%
    filter(districtDemographics == districtString) %>%
    pull(med_rent)
  
  return(as.character(med_rent))
}

chartData <- function(
  selectedState, selectedDistrict, categories, grouping = NULL, n_seed = NULL
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
  
  # Accounting for groupings in bar chart
  if (is.null(grouping) == FALSE) {
    counts$group <- grouping
  }
  
  return(counts)
}

pieChart <- function(
  selectedState,
  selectedDistrict,
  categories,
  category_strings = NULL,
  grouping = NULL,
  n_seed = NULL
  ) {
  
  counts <- chartData(selectedState, selectedDistrict, categories, n_seed)
  
  if (is.null(category_strings) == TRUE) {
    ggplot(data = counts, aes(x = "", y = Freq, fill = sampleVec)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_minimal() +
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
      theme_minimal() +
      scale_fill_discrete(name = "", labels = category_strings) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
  }
}

barChart <- function(
  selectedState,
  selectedDistrict,
  categories,
  category_strings = NULL,
  grouping = NULL,
  n_seed = NULL
  ) {
  
  counts <- chartData(
    selectedState, selectedDistrict, categories, grouping, n_seed
    )
  
  if (is.null(category_strings) == TRUE) {
    if (is.null(grouping) == TRUE) {
      ggplot(data = counts, aes(x = sampleVec, y = Freq)) +
        geom_bar(stat = "identity", position = "stack", aes(fill = sampleVec)) +
        theme_minimal() +
        scale_y_continuous(
          labels = comma_format(big.mark = ",", decimal.mark = ".")
          ) +
        theme(
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    } else {
      ggplot(data = counts[1:4, ], aes(x = group, y = Freq)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill = sampleVec)) +
        theme_minimal() +
        scale_fill_discrete(name = "") +
        scale_y_continuous(
          labels = comma_format(big.mark = ",", decimal.mark = ".")
        ) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    }
  } else {
    if (is.null(grouping) == TRUE) {
      ggplot(data = counts, aes(x = sampleVec, y = Freq)) +
        geom_bar(stat = "identity", position = "stack", aes(fill = sampleVec)) +
        theme_minimal() +
        scale_fill_discrete(name = "", labels = category_strings) +
        scale_y_continuous(
          labels = comma_format(big.mark = ",", decimal.mark = ".")
        ) +
        theme(
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    } else {
      ggplot(data = counts[1:4, ], aes(x = group, y = Freq)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill = sampleVec)) +
        theme_minimal() +
        scale_fill_discrete(name = "", labels = category_strings) +
        scale_y_continuous(
          labels = comma_format(big.mark = ",", decimal.mark = ".")
        ) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    }
  }
}

densityPlot <- function(selectedState, selectedDistrict) {
  ggplot(data = modelData, aes(x = med_rent)) +
    geom_density(alpha = .2, fill="#FF6655") +
    geom_vline(
      aes(xintercept = as.integer(getRent(selectedState, selectedDistrict))),
      colour = "red",
      linetype ="longdash",
      size = .8
      ) +
    theme_minimal() +
    scale_y_continuous(labels = comma_format(big.mar = ",", decimal.mark = ".")) +
    ylab("Density") +
    xlab("Median Rent")
}
