library(tidyverse)
library(randomForest)

# Uncomment for use in tree_func near bottom
# library(ggraph)
# library(igraph)

source("helpers/stringManipulator.R")

df <- read.csv("data/census/demographics/preprocessed/modelData.csv")

df$party <- df$party %>% sapply(as.factor)

for (col in colnames(df)) {
  if (class(df[[col]]) == "integer") {
    df[[col]] <- sapply(df[[col]], as.numeric)
  } 
}

# Removing "med_smoc_mort" and "med_smoc_no_mort" because "Over $4,000" is a
# character rather than a numerical variable.
df <- df[
  , !names(df) %in% c("med_smoc_mort", "med_smoc_no_mort"), drop = F
]

# scale.many function - standardizes whole columns
scale.many <- function(dat, column.nos) {
  nms <- names(dat)
  for(col in column.nos) {
    name <- paste(nms[col],".z", sep = "")
    dat[name] <- scale(dat[,col])
  }
  cat(paste("Scaled ", length(column.nos), " variable(s)\n"))
  dat
}

to.be.scaled <- 2:(length(colnames(df)) - 3)
dfScaled <- df %>% mutate_at(to.be.scaled, ~(scale(.) %>% as.vector))
row.names(dfScaled) <- dfScaled$districtDemographics

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

# NOTE: see helpers/model/modelInfo.R for model test metrics

test.preds <- model %>% predict(test %>% select(1:65))

test.preds <- data.frame(
  predicted = test.preds, actual = test$party
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