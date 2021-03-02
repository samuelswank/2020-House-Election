library(tidyverse)

# See data directory readme in master branch for a not on the data and their
# source
houseResults <- rjson::fromJSON(file = "data/houseResults.json")

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

hr <- hr %>% mutate(state = usdata::abbr2state(state_abb))
fips <- read_csv("data/stfipsab.csv") %>% select(stname, st)
fipsList <- split(fips, seq(nrow(fips)))
fipsList <- setNames(fipsList, fips$stname)

fipsCodes <- c()
for (i in 1:nrow(hr)) {
  fipsCodes[i] <- fipsList[[hr[i, "state"]]]$st
}
hr$fipsCode <- fipsCodes

flippedWinners <- c(
  "Stephanie Bice",
  "Carolyn Bourdeaux",
  "Michelle Fischbach",
  "Carlos Gimenez",
  "Yvette Herrell",
  "Ashley Hinson",
  "Young Kim",
  "Nancy Mace",
  "Nicole Malliotakis",
  "Mariannette Miller-Meeks",
  "Burgess Owens",
  "Maria Elvira Salazar",
  "Michelle Steel",
  "David Valadao"
  )

hr <- hr %>% add_column(flipped = NA)
for (i in 1:length(district)) {
  if(hr$winning_candidate[i] %in% flippedWinners) {
    hr$flipped[i] <- TRUE
  } else {hr$flipped[i] <- FALSE}
}

hr <- hr[
  ,
  c(
    "state",
    "state_abb",
    "fipsCode",
    "district",
    "winning_candidate",
    "party",
    "opponent",
    "opponent_party",
    "incumbent",
    "uncontested",
    "flipped"
    )
  ]