library(randomForest)

source("modelPreprocessing.R")
source("stringManipulator.R")

set.seed(42)
sample <- caTools::sample.split(dfScaled$party, SplitRatio = 0.8)

train <- subset(
  dfScaled[, !names(dfScaled) %in% c("flipped"), drop = F], sample == TRUE
  )

test  <- subset(
  dfScaled[, !names(dfScaled) %in% c("flipped"), drop = F], sample == FALSE
  )

set.seed(42)
model <- randomForest(party ~ ., data = train)

train.preds <- data.frame(
  predicted = model$predicted, actual = train$party
)

test.preds <- model %>% predict(test %>% select(1:65))

test.preds <- data.frame(
  predicted = test.preds, actual = testPA$party
)

preds <- bind_rows(train.preds, test.preds) %>%
  select(predicted, actual)

preds$district <- row.names(preds)
row.names(preds) <- 1:length(preds$district)

paStates <- predState(preds$district)
paDistricts <- predDistrict(preds$district)

preds$state <- paStates
preds$district <- paDistricts
preds <- preds[, c(4, 3, 2, 1)]

varImportances <- importance(model)
topTen <- varImportances[varImportances > 4, ] %>% sort(decreasing = TRUE)
topTen <- as.data.frame(topTen)
colnames(topTen)[1] <- "Importances"
rownames(topTen) <- c(
  "Walking/Public Transit",
  "Renter Occupied Units",
  "Natural-born Citizens",
  "Commuters by Car",
  "Owner Occupied Units",
  "Foreign-born Citizens",
  "Whites",
  "Asians",
  "Persons of Other Race",
  "Median Rent"
  )


