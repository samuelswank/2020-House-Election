library(randomForest)

source("modelPreprocessing.R")
source("stringManipulator.R")

'%!in%' <- function(x,y){!('%in%'(x,y))}

flippedScaled <- dfScaled[
  , !names(dfScaled) %in% c("districtDemographics"), drop = F
]

# set.seed(1930)
set.seed(1930)
propSample <- sample(
  1:nrow(flippedScaled %>% filter(party == "D" & flipped == FALSE)), 37
)

flippedScaled$district <- rownames(flippedScaled)
rownames(flippedScaled) <- NULL

propFlipped <- rbind(
  flippedScaled %>% subset(party == "R" & flipped == TRUE),
  flippedScaled %>% filter(party == "D" & flipped == FALSE) %>% .[propSample, ]
)

propFlipped <- propFlipped[sample(nrow(propFlipped)), ]
propFlipped <- propFlipped[, !names(propFlipped) %in% c("party"), drop = F]

rownames(propFlipped) <- NULL

trainProp <- propFlipped[1:40, ]
testProp  <- propFlipped[41:50, ]

trainProp$flipped <- as.character(trainProp$flipped)
testProp$flipped <- as.character(testProp$flipped)

trainProp$flipped <- factor(trainProp$flipped, levels = c("TRUE", "FALSE"))
testProp$flipped <- factor(testProp$flipped, levels = c("TRUE", "FALSE"))

# set.seed(1492)
set.seed(1492)
# ntree = 200
propModel <- randomForest(
  flipped ~ ., data = trainProp %>% select(1:65), ntree = 325
  )

flipped.train.preds <- data.frame(
  district = trainProp$district,
  predicted = propModel$predicted,
  actual = trainProp$flipped
  )

flipped.test.preds <- propModel %>% predict(testProp %>% select(1:65))
flipped.test.preds <- data.frame(
  district = testProp$district,
  predicted = flipped.test.preds,
  actual = testProp$flipped
)

test.flipped.cm <- yardstick::conf_mat(
  flipped.test.preds, truth = actual, estimate = predicted
)

summary(test.flipped.cm)

flipped.preds <- rbind(flipped.train.preds, flipped.test.preds)
for (col in colnames(flipped.preds)[2:3]) {
  flipped.preds[[col]] <- as.logical(flipped.preds[[col]])
  }

flippedResult <- function(selectedState, selectedDistrict) {
  result <- list()
  if (reverseDistrict(selectedState, selectedDistrict) %!in% flipped.preds$district) {
    result$in_sample <- FALSE
    result$actual    <- FALSE
    result$predicted <- FALSE
  } else {
    result$in_sample <- TRUE
    result$actual <- flipped.preds %>%
      filter(district == reverseDistrict(selectedState, selectedDistrict)) %>%
      .$actual
    
    result$predicted <- flipped.preds %>%
      filter(district == reverseDistrict(selectedState, selectedDistrict)) %>%
      .$predicted
  }
  return(result)
}
