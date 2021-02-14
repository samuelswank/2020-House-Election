library(tidyverse)

df <- read.csv("data/census/demographics/preprocessed/modelData.csv")

df$party <- df$party %>% sapply(as.factor)

for (col in colnames(df)) {
  if (class(df[[col]]) == "integer") {
    df[[col]] <- sapply(df[[col]], as.numeric)
  } 
}

df <- df[
  , !names(df) %in% c("med_smoc_mort", "med_smoc_no_mort"), drop = F
  ]

scale.many <- function(dat, column.nos) {
  nms <- names(dat)
  for(col in column.nos) {
    name <- paste(nms[col],".z", sep = "")
    dat[name] <- scale(dat[,col])
  }
  cat(paste("Scaled ", length(column.nos), " variable(s)\n"))
  dat
}

to.be.scaled <- 2:(length(colnames(paData)) - 3)
dfScaled <- df %>% mutate_at(to.be.scaled, ~(scale(.) %>% as.vector))
row.names(dfScaled) <- dfScaled$districtDemographics


