library(tidyverse)
library(tidycensus)
source("houseResults.R")
source("stringManipulator.R")

tractTable <- read_csv("data/TRACT2CD115_table.csv", col_names = F)

colnames(tractTable) <- c(
  "Tract GeoID",
  "St",
  "StAbCD",
  "StCty",
  "Tract Code",
  "CD115",
  "ZIP",
  "Pop 2010",
  "TractPart",
  "Land SqMi",
  "Water SqMi",
  "Pop ACS 2016",
  "$MHI ACS 2016"
  )

flippedTable <- tractTable %>% filter(St %in% flippedStates)
flippedTracts <- flippedTable %>%
  subset(
    (St == "CA" & CD115 == "39") | (St == "CA" & CD115 == "48") |
    (St == "FL" & CD115 == "26") | (St == "FL" & CD115 == "27") |
    (St == "IA" & CD115 == "1") |
    (St == "MN" & CD115 == "7") |
    (St == "NM" & CD115 == "2") |
    (St == "SC" & CD115 == "1") |
    (St == "OK" & CD115 == "5") |
    (St == "UT" & CD115 == "4") |
    (St == "GA" & CD115 == "7")
  )

demog_vars <- c(
  Total = "B01003_001",
  Male = "B01001_002",
  Female = "B01001_026",
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  Islander = "B03002_007",
  Other = "B03002_008",
  Multiple = "B03002_009",
  Hispanic = "B03002_012"
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
  VotingAgeAssociates = "B29002_007",
  VotingAgeBachelors = "B29002_008",
  VotingAgeGradorProf = "B29002_009",
  BelowPoverty = "B29003_002",
  AbovePoverty = "B29003_003",
  MedianIncome = "B29004_001"
)

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
  ForeignBorn = "B27020_008"
)

