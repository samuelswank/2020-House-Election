library(tidyverse)
library(scales)
library(gridExtra)
library(randomForest)

'%!in%' <- function(x,y){!('%in%'(x,y))}

modelData <- read_csv("data/census/demographics/preprocessed/modelData.csv")

colnames(modelData)[1] <- "district"
modelData <- as.data.frame(modelData)
rownames(modelData) <- modelData$district
modelData <- modelData[, 2:(ncol(modelData) - 1)]

modelData[272, "party"]   <- "R"

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
      "Georgia 6", "North Carolina 2", "North Carolina 6"
      )
    ) {flipped[i] <- "Flipped Democrat"}
  else if (rownames(gmDistance)[i] %in% flippedRepublican) {
    flipped[i] <- "Flipped Republican"
  } else if (as.character(df$party[i]) == "D") {
    flipped[i] <- "Democrat - Not Flipped"
  } else if (as.character(df$party[i]) == "R") {
    flipped[i] <- "Republican - Not Flipped"
    }
}


gmDistance$flipped <- flipped
gmDistance$flipped <- gmDistance$flipped %>% sapply(as.factor)

ggplot(gmDistance, aes(x = flipped, y = associates)) +
  geom_boxplot() +
  stat_summary(geom = "point")

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
# 
# flippedDemocrat <- c("Georgia 7", "North Carolina 2", "North Carolina 6")
# democratPlots <- list()
# 
# for (col_ in colnames(modelData)[1:(ncol(modelData) - 1)]) {
#   democratPlots[[col_]] <- grid.arrange(
#     densityPlot(flippedDemocrat[1], col_),
#     densityPlot(flippedDemocrat[2], col_),
#     densityPlot(flippedDemocrat[3], col_),
#     nrow = 3
#   )
# }
# 
# flippedRepublican <- c(
#   "California 21",
#   "California 39",
#   "California 48",
#   "Florida 26",
#   "Florida 27",
#   "Iowa 1",
#   "Iowa 2",
#   "New Mexico 2",
#   "New York 11",
#   "New York 22",
#   "Oklahoma 5",
#   "Utah 4"
# )
# 
# republicanPlots <- list()
# 
# for (col_ in colnames(modelData)[1:(ncol(modelData) - 1)]) {
#   republicanPlots[[col_]] <- grid.arrange(
#     densityPlot(flippedRepublican[1], col_),
#     densityPlot(flippedRepublican[2], col_),
#     densityPlot(flippedRepublican[3], col_),
#     densityPlot(flippedRepublican[4], col_),
#     densityPlot(flippedRepublican[5], col_),
#     densityPlot(flippedRepublican[6], col_),
#     densityPlot(flippedRepublican[7], col_),
#     densityPlot(flippedRepublican[8], col_),
#     densityPlot(flippedRepublican[9], col_),
#     densityPlot(flippedRepublican[10], col_),
#     densityPlot(flippedRepublican[11], col_),
#     densityPlot(flippedRepublican[12], col_),
#     nrow = 3,
#     ncol = 4
#   )
# }


