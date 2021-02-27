library(randomForest)

source("modelPreprocessing.R")
source("stringManipulator.R")

flippedScaled <- dfScaled[
  , !names(dfScaled) %in% c("districtDemographics"), drop = F
]

flipped    <- length((flippedScaled %>% subset(flipped == TRUE))$flipped)
notFlipped <- length((flippedScaled %>% subset(flipped == FALSE))$flipped)

set.seed(1492)
propSample <- sample(
  1:nrow(flippedScaled %>% subset(party == "D" & flipped == FALSE)), 86
)

propFlipped <- rbind(
  flippedScaled %>% subset(party == "R" & flipped == TRUE),
  flippedScaled[propSample, ]
)

propFlipped <- propFlipped[sample(nrow(propFlipped)), ]
propFlipped <- propFlipped[, !names(propFlipped) %in% c("party"), drop = F]

trainProp <- propFlipped[1:80, ]
testProp  <- propFlipped[81:99, ]

trainProp$flipped <- as.character(trainProp$flipped)
testProp$flipped <- as.character(testProp$flipped)

trainProp$flipped <- factor(trainProp$flipped, levels = c("TRUE", "FALSE"))
testProp$flipped <- factor(testProp$flipped, levels = c("TRUE", "FALSE"))

set.seed(1812)
propModel <- randomForest(flipped ~ ., data = trainProp, ntree = 9)

flipped.train.preds <- data.frame(
  predicted = propModel$predicted, actual = trainProp$flipped
)

flipped.test.preds <- propModel %>% predict(testProp %>% select(1:64))
flipped.test.preds <- data.frame(
  predicted = flipped.test.preds, actual = testProp$flipped
)

flipped.preds <- rbind(flipped.train.preds, flipped.test.preds)