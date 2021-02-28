library(randomForest)

source("modelPreprocessing.R")
source("stringManipulator.R")

'%!in%' <- function(x,y){!('%in%'(x,y))}

flippedScaled <- dfScaled[
  , !names(dfScaled) %in% c("districtDemographics"), drop = F
]

flipped    <- length((flippedScaled %>% subset(flipped == TRUE))$flipped)
notFlipped <- length((flippedScaled %>% subset(flipped == FALSE))$flipped)

set.seed(1492)
propSample <- sample(
  1:nrow(flippedScaled %>% subset(party == "D" & flipped == FALSE)), 87
)

flippedScaled$district <- rownames(flippedScaled)
rownames(flippedScaled) <- NULL

propFlipped <- rbind(
  flippedScaled %>% subset(party == "R" & flipped == TRUE),
  flippedScaled[propSample, ]
)

propFlipped <- propFlipped[sample(nrow(propFlipped)), ]
propFlipped <- propFlipped[, !names(propFlipped) %in% c("party"), drop = F]

rownames(propFlipped) <- NULL

trainProp <- propFlipped[1:80, ]
testProp  <- propFlipped[81:100, ]

trainProp$flipped <- as.character(trainProp$flipped)
testProp$flipped <- as.character(testProp$flipped)

trainProp$flipped <- factor(trainProp$flipped, levels = c("TRUE", "FALSE"))
testProp$flipped <- factor(testProp$flipped, levels = c("TRUE", "FALSE"))

# for (i in 1:25) {
#   set.seed(1812)
#   treeModel <- randomForest(flipped ~ ., data = trainProp, ntree = i)
#   sprintf("%d\n-----------------------------------------------------\n", i)
#   flipped.train.preds <- data.frame(
#     predicted = treeModel$predicted, actual = trainProp$flipped
#   )
#   
#   train.flipped.cm <- yardstick::conf_mat(
#     flipped.train.preds, truth = actual, estimate = predicted
#   )
#   
#   print(i)
#   print(
#     summary(train.flipped.cm) %>%
#       subset(
#         .metric == "accuracy" |
#           .metric == "mcc" |
#           .metric == "precision" |
#           .metric == "recall" |
#           .metric == "f_meas"
#       )
#   )
# }

set.seed(1812)
propModel <- randomForest(
  flipped ~ ., data = trainProp %>% select(1:65), ntree = 25
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

flipped.preds <- rbind(flipped.train.preds, flipped.test.preds)
for (col in colnames(flipped.preds)) {
  flipped.preds[[col]] <- as.logical(flipped.preds[[col]])
  }

flippedResult <- function(selectedState, selectedDistrict) {
  result <- list()
  if (reverseDistrict(selectedState, selectedDistrict) %!in% flipped.preds$district) {
    result$actual    <- FALSE
    result$predicted <- FALSE
  } else {
    result$actual <- flipped.preds %>%
      filter(district == reverseDistrict(selectedState, selectedDistrict)) %>%
      .$actual[1]
    
    result$predicted <- flipped.preds %>%
      filter(district == reverseDistrict(selectedState, selectedDistrict)) %>%
      .$predicted[1]
  }
  return(result)
}
