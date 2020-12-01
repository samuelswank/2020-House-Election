library(dotenv)
library(tidyverse)
library(tidycensus)
library(usdata)
source("houseResults.R")
source("stringManipulator.R")

load_dot_env(".env")
census_api_key(Sys.getenv("CENSUS_KEY"))

tractTable <- read_csv("data/TRACT2CD115_table.csv", col_names = F)[
  , c(1, 2, 6, 9, 10, 11)
  ]

colnames(tractTable) <- c(
  "Tract GeoID",
  "St",
  "CD115",
  "TractPart",
  "Land SqMi",
  "Water SqMi"
  )

flippedTable <- tractTable %>% filter(St %in% flippedStates)
flippedTracts <- flippedTable %>%
  subset(
    (St == "CA" & CD115 == "21") | 
    (St == "CA" & CD115 == "39") | 
    (St == "CA" & CD115 == "48") |
    (St == "FL" & CD115 == "26") | (St == "FL" & CD115 == "27") |
    (St == "IA" & CD115 == "01") |
    (St == "MN" & CD115 == "07") |
    (St == "NM" & CD115 == "02") |
    (St == "SC" & CD115 == "01") |
    (St == "OK" & CD115 == "05") |
    (St == "UT" & CD115 == "04") |
    (St == "GA" & CD115 == "07")
  )

demographic_vars <- c(
  Male = "B01001_002",
  Female = "B01001_026",
  White = "B02001_002",
  Black = "B02001_003",
  Native = "B02001_004",
  Asian = "B02001_005",
  Islander = "B02001_006",
  Other = "B02001_007",
  Multiple = "B02001_008",
  Hispanic = "B03001_003"
)

vap_vars <- c(
  VotingAgePop = "B29001_001",
  VotingAgePop18to29 = "B29001_002",
  VotingAgePop30to44 = "B29001_003",
  VotingAgePop45to64 = "B29001_004",
  VotingAgePop65Over = "B29001_005",
  VotingAge9thto12th = "B29002_003",
  VotingAgeHighschool = "B29002_004",
  VotingAgeSomecollege = "B29002_005",
  VotingAgeAssociates = "B29002_006",
  VotingAgeBachelors = "B29002_007",
  VotingAgeGradorProf = "B29002_008",
  BelowPoverty = "B29003_002",
  AbovePoverty = "B29003_003",
  MedianIncome = "B29004_001"
)

# Unused

labor_vars <- c(
  WorkingAgePop = "B23025_001",
  LaborForce = "B23025_002",
  CivilianLaborForce = "B23025_003",
  Employed = "B23025_004",
  Unemployment = "B23025_005",
  ArmedForces = "B23025_006",
  OutsideLaborForce = "B23025_007"
)

health_vars = c(
  NativeBorn = "B27020_002",
  NBHealthInsurance = "B27020_003",
  NBPrivateHealthInsurance = "B27020_004",
  NBPublicHealthInsurance = "B27020_005",
  NBNoHealthInsurance = "B27020_06",
  ForeignBorn = "B27020_008",
  FBHealthInsurance = "B27020_009",
  FBPrivateHealthInsurance = "B27020_010",
  FBPublicHealthInsurance = "B27020_011",
  FBNoHealthInsurance = "B27020_012"
)

demographicData <- function(selectedDistrict) {
  zero <- FALSE
  if (selectedDistrict %in% districtChoices[6:12]) {zero <- TRUE}
  
  demographics <- get_acs(
    geography = "tract",
    variables = demographic_vars,
    cache_table = TRUE,
    year = 2018,
    survey = "acs5",
    state = selectedDistrict %>% stateString() %>% state2abbr()
  )
  
  flippedDemographics <- demographics[, c(1:4)] %>% 
    filter(
      GEOID %in% filter(
        flippedTracts, CD115 == districtString(selectedDistrict, zero = zero)
        )$`Tract GeoID`
      )
  
  flippedDemographics <- flippedDemographics %>% spread(variable, estimate)
  colnames(flippedDemographics)[9] <- "Multiple Races"
  colnames(flippedDemographics)[11] <- "Other Race"
  
  return(flippedDemographics[, c(1, 2, 8, 5, 6, 12, 4, 10, 7, 3, 9, 11)])
}

demographicTable <- function(selectedDistrict, demographicData) {
  sumsDemographics <- colSums(demographicData[, 5:12])
  
  return(data.frame(Population = sumsDemographics))
}

sexRatio <- function(selectedDistrict, demographicData) {
  sumsDemographics <- colSums(demographicData[, 3:4])
  return(round(100 *(sumsDemographics["Male"] / sumsDemographics["Female"]), 2))
}


vaTable <- function(selectedDistrict) {
  zero <- FALSE
  if (selectedDistrict %in% districtChoices[6:12]) {zero <- TRUE}
  
  va <- get_acs(
    geography = "tract",
    variables = vap_vars,
    cache_table = TRUE,
    year = 2018,
    survey = "acs5",
    state = selectedDistrict %>% stateString() %>% state2abbr()
  )
  
  flippedVA <- va[, c(1:4)] %>% 
    filter(
      GEOID %in% filter(
        flippedTracts, CD115 == districtString(selectedDistrict, zero = zero)
      )$`Tract GeoID`
    )
  
  flippedVA <- (flippedVA %>% spread(variable, estimate))
  flippedVA <- flippedVA[, 3:16]
  colnames(flippedVA) <- c(
    "Above Poverty",
    "Below Poverty",
    "Median Income",
    "Some High School No Diploma or Equivalent",
    "Associates Degree",
    "Bachelors Degree",
    "Graduate or Professional Degree",
    "High School Diploma or Equivalent",
    "Voting Age Population",
    "Population Age 18 to 29",
    "Population Age 30 to 44",
    "Population Age 45 to 64",
    "Population Age 65 and Older",
    "Some College"
    )
  
  flippedVA <- flippedVA[, c(9, 10, 11, 12, 13, 3, 2, 1, 4, 8, 14, 5, 6, 7)]
  
  statsVA <- colSums(flippedVA[, -6])
  mi <- median(flippedVA$`Median Income`, na.rm = TRUE)
  statsVA <- append(statsVA, mi, 5)

  df <- data.frame(Statistic = statsVA)
  rownames(df)[6] <- "Median Income"
  return(df)
}
