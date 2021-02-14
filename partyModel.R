source("modelPreprocessing.R")
source("stringManipulator.R")

set.seed(42)
sample <- caTools::sample.split(paScaled$party, SplitRatio = 0.8)

trainPA <- subset(
  dfScaled[, !names(dfScaled) %in% c("flipped"), drop = F], sample == TRUE
  )

testPA  <- subset(
  dfScaled[, !names(dfScaled) %in% c("flipped"), drop = F], sample == FALSE
  )

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
row.names(pa.preds) <- 1:length(pa.preds$district)

paStates <- predState(pa.preds$district)
paDistricts <- predDistrict(pa.preds$district)

pa.preds$state <- paStates
pa.preds$district <- paDistricts
pa.preds <- pa.preds[, c(4, 3, 2, 1)]
