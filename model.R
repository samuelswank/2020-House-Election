library(tidyverse)

df <- read.csv("data/census/demographics/preprocessed/modelData.csv")

df$party <- df$party %>% sapply(as.factor)

for (col in colnames(df)) {
  if (class(df[[col]]) == "integer") {
    df[[col]] <- sapply(df[[col]], as.numeric)
  } 
}

paData <- df[
  -269, !names(df) %in% c("med_smoc_mort", "med_smoc_no_mort", "flipped"), drop = F
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

to.be.scaled <- 2:(length(colnames(paData)) - 2)
paScaled <- paData %>% mutate_at(to.be.scaled, ~(scale(.) %>% as.vector))
row.names(paScaled) <- paScaled$districtDemographics

set.seed(42)
sample <- caTools::sample.split(paScaled$party, SplitRatio = 0.8)

trainPA <- subset(paScaled, sample == TRUE)
testPA  <- subset(paScaled, sample == FALSE)

set.seed(42)
pa.model <- randomForest(party ~ ., data = trainPA)

pa.train.preds <- data.frame(
  predicted = pa.model$predicted, actual = trainPA$party
)

pa.test.preds <- pa.model %>% predict(testPA %>% select(1:65))

pa.test.preds <- data.frame(
  predicted = pa.test.preds, actual = testPA$party
)

pa.preds <- bind_rows(pa.train.preds, pa.test.preds) %>%
  select(predicted, actual)

pa.preds$district <- row.names(pa.preds)
pa.preds <- pa.preds[, c(3, 1)]



