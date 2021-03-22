library(tidyverse)

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

densityPlot <- function(selectedDistrict, selectedStat) {
  dp <- ggplot(data = modelData, aes(x = modelData[, selectedStat])) +
    geom_density(alpha = .2) +
    geom_vline(
      aes(xintercept = getStat(selectedDistrict, selectedStat)),
      colour = "black",
      linetype ="longdash",
      size = .8
    ) +
    theme_minimal() +
    scale_y_continuous(labels = comma_format(big.mar = ",", decimal.mark = ".")) +
    ggtitle(selectedDistrict) +
    ylab("Density") +
    xlab(selectedStat)
    
  return(dp)
}

flippedDemocrat <- c("Georgia 7", "North Carolina 2", "North Carolina 6")
democratPlots <- list()

for (col_ in colnames(modelData)[1:(ncol(modelData) - 1)]) {
  democratPlots[[col_]] <- list()
  for (voterDistrict in flippedDemocrat) {
    democratPlots[[col_]][[voterDistrict]] <- densityPlot(voterDistrict,  col_)
  }
}


