library(tidyverse)
library(scales)
library(gridExtra)
source("helpers/model/partyModel.R")

'%!in%' <- function(x,y){!('%in%'(x,y))}

modelData <- read_csv("data/census/demographics/preprocessed/modelData.csv")

colnames(modelData)[1] <- "district"
modelData <- as.data.frame(modelData)
rownames(modelData) <- modelData$district
modelData <- modelData[, 2:(ncol(modelData) - 1)]

modelData[272, "party"]   <- "R"

# densityPlot <- function(selectedDistrict, selectedStat) {
#   dp <- ggplot(data = modelData, aes(x = modelData[, selectedStat])) +
#     geom_density(alpha = .2) +
#     geom_vline(
#       aes(xintercept = getStat(selectedDistrict, selectedStat)),
#       colour = "black",
#       linetype ="longdash",
#       size = .8
#     ) +
#     theme_minimal() +
#     scale_y_continuous(labels = comma_format(big.mar = ",", decimal.mark = ".")) +
#     ggtitle(selectedDistrict) +
#     ylab("Density") +
#     xlab(selectedStat)
#     
#   return(dp)
# }

getStat <- function(selectedDistrict, selectedStat) {
  
  selectedStat <- modelData[selectedDistrict, ] %>% pull(selectedStat)
  
  return(selectedStat)
}

globalMaximum <- function(selectedStat) {
  statDensity <- density(modelData[[selectedStat]], na.rm = TRUE)
  statDensity.frame <- data.frame(x = statDensity$x, y = statDensity$y)
  
  return(statDensity.frame %>% subset(y == max(y)) %>% .[1, "x"] %>% round(.))
}

globalMaxima <- list()

for (col_ in colnames(modelData)[1:(ncol(modelData) - 1)]) {
  globalMaxima[[col_]] <- globalMaximum(col_)
}

democratDistance <- data.frame()

for (col_ in colnames(modelData)[1:(ncol(modelData) - 1)]) {
  for (voterDistrict in c("Georgia 6", "North Carolina 2", "North Carolina 6")) {
    democratDistance[voterDistrict, col_] <- (
      getStat(voterDistrict, col_) - globalMaxima[[col_]]
      )
  }
}

republicanDistance <- data.frame()
flippedRepublican <- c(
  "California 21",
  "California 39",
  "California 48",
  "Florida 26",
  "Florida 27",
  "Iowa 1",
  "Iowa 2",
  "New Mexico 2",
  "New York 11",
  "New York 22",
  "Oklahoma 5",
  "Utah 4"
)

for (col_ in colnames(modelData)[1:(ncol(modelData) - 1)]) {
  for (voterDistrict in flippedRepublican) {
    republicanDistance[voterDistrict, col_] <- (
      getStat(voterDistrict, col_) - globalMaxima[[col_]]
      )
  }
}


gmDistance <- data.frame()

for (col_ in colnames(modelData)[1:(ncol(modelData) - 1)]) {
  for(voterDistrict in rownames(modelData)) {
    gmDistance[voterDistrict, col_] <- (
      getStat(voterDistrict, col_) - globalMaxima[[col_]]
    )
  }
}

flipped <- c()

for (i in 1:nrow(gmDistance)) {
  if (
    rownames(gmDistance)[i] %in% c(
      "Georgia 7", "North Carolina 2", "North Carolina 6"
      ) |
    rownames(gmDistance)[i] %in% flippedRepublican
    ) {flipped[i] <- "Flipped"}
  else if (as.character(df$party[i]) == "D") {
    flipped[i] <- "Democrat - Not Flipped"
    
  } else if (as.character(df$party[i]) == "R") {
    flipped[i] <- "Republican - Not Flipped"
  }
}

gmDistance$flipped <- flipped
gmDistance$flipped <- gmDistance$flipped %>% sapply(as.factor)
gmDistance <- gmDistance[
  , !names(gmDistance) %in% c("med_smoc_mort", "med_smoc_no_mort")
  ]

gdScaled <- gmDistance %>%
  mutate_at(1:(ncol(gmDistance) - 1), ~(scale(.) %>% as.vector))

gdScaled$flipped <- gdScaled$flipped %>%
  factor(
    levels = c("Flipped", "Republican - Not Flipped", "Democrat - Not Flipped")
    )

rownames(gdScaled) <- rownames(gmDistance)

distanceSummary <- data.frame(
  Min = rep(NA, ncol(gmDistance) - 1),
  First= rep(NA, ncol(gmDistance) - 1),
  Median = rep(NA, ncol(gmDistance) - 1),
  Mean = rep(NA, ncol(gmDistance) - 1),
  Third = rep(NA, ncol(gmDistance) - 1),
  Max = rep(NA, ncol(gmDistance) - 1)
)

for (i in 1:(ncol(gmDistance) -1)) {
  distanceSummary[i, ] <- summary(gmDistance %>% .[, colnames(gmDistance)[i]])
}

rownames(distanceSummary) <- colnames(gmDistance)[1:(ncol(gmDistance) - 1)]

republicanSummary <- data.frame(
  Min = rep(NA, ncol(gmDistance) - 1),
  First= rep(NA, ncol(gmDistance) - 1),
  Median = rep(NA, ncol(gmDistance) - 1),
  Mean = rep(NA, ncol(gmDistance) - 1),
  Third = rep(NA, ncol(gmDistance) - 1),
  Max = rep(NA, ncol(gmDistance) - 1)
)

for (i in 1:(ncol(gmDistance) -1)) {
  republicanSummary[i, ] <- summary(
    gmDistance %>%
      subset(flipped == "Republican - Not Flipped") %>%
      .[, colnames(gmDistance)[i]]
    )
}

rownames(republicanSummary) <- rownames(distanceSummary)

democratSummary <- data.frame(
  Min = rep(NA, ncol(gmDistance) - 1),
  First= rep(NA, ncol(gmDistance) - 1),
  Median = rep(NA, ncol(gmDistance) - 1),
  Mean = rep(NA, ncol(gmDistance) - 1),
  Third = rep(NA, ncol(gmDistance) - 1),
  Max = rep(NA, ncol(gmDistance) - 1)
)

for (i in 1:(ncol(gmDistance) -1)) {
  democratSummary[i, ] <- summary(
    gmDistance %>%
      subset(flipped == "Democrat - Not Flipped") %>%
      .[, colnames(gmDistance)[i]]
  )
}

rownames(democratSummary) <- rownames(distanceSummary)

flippedSummary <- data.frame(
  Min = rep(NA, ncol(gmDistance) - 1),
  First= rep(NA, ncol(gmDistance) - 1),
  Median = rep(NA, ncol(gmDistance) - 1),
  Mean = rep(NA, ncol(gmDistance) - 1),
  Third = rep(NA, ncol(gmDistance) - 1),
  Max = rep(NA, ncol(gmDistance) - 1)
)

for (i in 1:(ncol(gmDistance) -1)) {
  flippedSummary[i, ] <- summary(
    gmDistance %>% subset(flipped == "Flipped") %>% .[, colnames(gmDistance)[i]]
  )
}

rownames(flippedSummary) <- rownames(distanceSummary)

comparison.data.frame <- function(statistic) {
  frame <- distanceSummary[statistic, ] %>%
    rbind(democratSummary[statistic, ]) %>%
    rbind(republicanSummary[statistic, ]) %>%
    rbind(flippedSummary[statistic, ])
  
  rownames(frame) <- c(
    "All", "Democrat - Not Flipped", "Republican - Not Flipped", "Flipped"
    )
  
  return(frame)
}
