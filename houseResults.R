library(rjson)
library(tidyverse)
library(usdata)

houseResults <- fromJSON(file = "data/houseResults.json")

stateAbb           <- c()
district           <- c()
candidate          <- c()
party              <- c()
percentage         <- c()
opponent           <- c()
opponentParty      <- c()
opponentPercentage <- c()
incumbent          <- c()


for (i in 1:length(houseResults)) {
  stateAbb[i] <- houseResults[[i]]$stateCode
  district[i] <- paste(
    "Congressional District", toString(houseResults[[i]]$districtNumber)
    )
  for (result in houseResults[[i]]$results) {
    if (result$isWinner == TRUE) {
      candidate[i] <- paste(
        result$candidate$firstName, result$candidate$lastName
        )
      party[i] <- result$partyCode
      if (result$votes$percentage == 0) {percentage[i] <- 100}
      else {percentage[i] <- result$votes$percentage}
      
      if (result$isIncumbent == TRUE) {incumbent[i] <- TRUE}
      else {incumbent[i] <- FALSE}
      
    } else if (
      result$isWinner == FALSE & (
        result$partyCode == "R" |
        result$partyCode == "D" |
        result$votes$percentage >= 10
        )
      ) {
      opponent[i] <- paste(
        result$candidate$firstName, result$candidate$lastName
      )
      opponentParty[i] <- result$partyCode
      opponentPercentage[i] <- result$votes$percentage
    }
  }
}

hr <- data.frame(
  state_abb = stateAbb, 
  district = district, 
  winning_candidate = candidate,
  party = party,
  percentage = percentage,
  opponent = opponent,
  opponent_party = opponentParty,
  opponent_percentage = opponentPercentage,
  incumbent = incumbent
  )

ongoingRaces <- filter(hr, is.na(winning_candidate) == TRUE)

uncontested <- c()

for (i in 1:nrow(hr)) {
  if (is.na(hr[i, "opponent"]) == TRUE) {
    uncontested[i] <- TRUE
    hr[i, "opponent_percentage"] <- 0.00
    }
  else {uncontested[i] <- FALSE}
}

hr <- cbind(hr, uncontested)

hr <- hr %>% mutate(state = abbr2state(state_abb))
fips <- read_csv("data/stfipsab.csv") %>% select(stname, st)
fipsList <- split(fips, seq(nrow(fips)))
fipsList <- setNames(fipsList, fips$stname)
fipsCodes <- c()
for (i in 1:nrow(hr)) {
  fipsCodes[i] <- fipsList[[hr[i, "state"]]]$st
}
hr$fipsCode <- fipsCodes
hr <- hr[, c(11, 1, 12, 2, 3, 4, 5, 6, 7, 8, 9, 10)]

flippedStates <- c("MN", "NM", "IA", "CA", "SC", "NY", "UT", "FL", "OK", "GA")

flippedResults <- filter(hr, state_abb %in% flippedStates)

flippedWinners <- c(
  "Michelle Fischbach",
  "Yvette Herrell",
  "Ashley Hinson",
  "Young Kim",
  "Nancy Mace",
  "Maria Elvira Salazar",
  "Michelle Steel",
  "Stephanie Bice",
  "Burgess Owens",
  "Carlos Gimenez",
  "Carolyn Bourdeaux"
  )

flippedDistricts <- filter(
  flippedResults, winning_candidate %in% flippedWinners
)

districtChoices <- c()

for (i in 1:nrow(flippedDistricts)) {
  districtChoices[i] <- paste(
    flippedDistricts[i, "state"],
    "District",
    str_remove(flippedDistricts[i, "district"], "Congressional District ")
  )
}