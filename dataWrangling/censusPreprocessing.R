library(tidyverse)
source("dataWrangling/houseResults.R")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Accounting for single district states

atLarge <- character()

for (f in list.files("data/census/demographics/raw/")) {
  if (substrRight(f, 9) == "Large.csv") {
    # Accounting for North and South Dakota
    if (
      strsplit(f, split = "_")[[1]][1] == "North" |
      strsplit(f, split = "_")[[1]][1] == "South"
    ) {
      atLarge <- atLarge %>% append(
        paste(
          strsplit(f, split = "_")[[1]][1], strsplit(f, split = "_")[[1]][2]
        )
      )
    } else {atLarge <- atLarge %>% append(strsplit(f, split = "_")[[1]][1])}
  }
}



stateDemographics <- function(state) {
  if (state %in% atLarge) {
    if (length(strsplit(state, split = " ")[[1]]) == 2) {
      state <- paste(
        strsplit(state, split = " ")[[1]][1],
        strsplit(state, split = " ")[[1]][2],
        sep = "_"
      )
    } else if (length(strsplit(state, split = " ")[[1]]) == 3) {
      state <- paste(
        strsplit(state, split = " ")[[1]][1],
        strsplit(state, split = " ")[[1]][2],
        strsplit(state, split = " ")[[1]][3],
        sep = "_"
      )
    }
    wholeState <- read_csv(
      paste(
        paste("data/census/demographics/raw/", state, sep = ""),
        "District", "At", "Large.csv", sep = "_"
      )
    )
  } else {
    if (length(strsplit(state, split = " ")[[1]]) == 2) {
      state = paste(
        strsplit(state, split = " ")[[1]][1],
        strsplit(state, split = " ")[[1]][2],
        sep = "_"
      )
    } else if (length(strsplit(state, split = " ")[[1]]) == 3) {
      state <- paste(
        strsplit(state, split = " ")[[1]][1],
        strsplit(state, split = " ")[[1]][2],
        strsplit(state, split = " ")[[1]][3],
        sep = "_"
      )
    }
    wholeState <- read_csv(
      paste(
        paste("data/census/demographics/raw/", state, sep = ""),
        "District", "all.csv", sep = "_"
      )
    )
  }
  
  if (state %>% sjmisc::str_contains("_") == TRUE) {
    if (length(strsplit(state, split = "_")[[1]]) == 3) {
      state <- paste(
        strsplit(state, split = "_")[[1]][1],
        strsplit(state, split = "_")[[1]][2],
        strsplit(state, split = "_")[[1]][3]
      )
    } else {
      state <- paste(
        strsplit(state, split = "_")[[1]][1],
        strsplit(state, split = "_")[[1]][2]
      )
    }
  }
  
  wholeState <- wholeState[, 2:length(colnames(wholeState))]
  
  for (col in wholeState %>% select(ends_with("Estimate")) %>% colnames()) {
    wholeState[, col] <- wholeState[, col] %>%
      lapply(function(x) as.numeric(as.character(x)))
  }
  
  for (col in wholeState %>% select(ends_with("MOE")) %>% colnames()) {
    wholeState[, col] <- wholeState[, col] %>% 
      lapply(function(x) abs(as.numeric(gsub("[^0-9.-]", "", x))))
  }
  
  wholeState <- wholeState %>% 
    gather(district, statistic, 3:length(colnames(wholeState)))
  
  # For first pass solution,
  # MOE will simply be dropped
  wholeState <- wholeState %>% subset(type = "Estimate")
  
  if (state %in% atLarge) {
    wholeState <- wholeState %>%
      separate(district, c("state", "district1", "district2", "type"))
    
    wholeState$district <- paste(wholeState$district1, wholeState$district2)
    wholeState <- wholeState[, c(3, 8, 1, 2, 6, 7)]
  } else {
    wholeState <- wholeState %>%
      separate(district, c("state", "district", "type"))
    
    wholeState$district <- as.numeric(wholeState$district)
    wholeState <- wholeState[, c(3, 4, 1, 2, 5, 6)]
  }
  
  wholeState <- wholeState[, !(names(wholeState) %in% c("type"))]
  
  for (i in 1:length(wholeState$state)) {wholeState[i, "state"] <- state}
  
  return(wholeState)
}

statesDC <- state.name
demographics <- list()

for (state in statesDC) {
  demographics[[state]] <- stateDemographics(state)
}

wholeCountry <- do.call("rbind", demographics)
wholeCountry$district1 <- paste0(wholeCountry$state, " ", wholeCountry$district)
wholeCountry <- wholeCountry[, c(6, 3, 4, 5)]
colnames(wholeCountry)[1] <- "district"

n <- 436
# 66 variables 

districtDemographics <- data.frame(
  district = rep(NA, n),
  # Population Density (Total population / Land Area)
  # pop_density = rep(NA, n),
  
  # Sex and Age
  
  # Sex Ratio
  sex_ratio = rep(NA, n),
  # Minors
  minors = rep(NA, n),
  # Voting Age Population
  voting_age_pop = rep(NA, n),
  # Seniors
  seniors = rep(NA, n),
  # Median age (years)
  med_age = rep(NA, n),
  
  # Race
  
  # Whites
  white = rep(NA, n),
  # Blacks
  black = rep(NA, n),
  # American Indians
  amerindian = rep(NA, n),
  # Pacific Islanders
  islander = rep(NA, n), 
  # Asians
  asian = rep(NA, n), 
  # Other
  other_race = rep(NA, n), 
  # Multiple Races
  multiracial = rep(NA, n),
  
  # Hispanic or Latino and Race
  
  # Hispanic
  hispanic = rep(NA, n),
  # Mexican
  mexican = rep(NA, n),
  # Puerto Rican
  puerto_rican = rep(NA, n),
  # Cuban
  cuban = rep(NA, n),
  # Other Hispanic
  other_hispanic = rep(NA, n),
  # Natural born citizen
  
  # Place of Birth
  
  # Natural Born Citizen
  natural_born_citizen = rep(NA, n),
  # Born in State of residence
  born_in_state = rep(NA, n),
  # Born in Different state
  born_out_of_state = rep(NA, n),
  # Born in US Territory, or born abroat to American parent(s)
  born_abroad = rep(NA, n),
  # Foreign Born
  foreign_born = rep(NA, n),
  # Disability Status of the Civilian Noninstitutionalized Population
  
  # (Voting Age) Disabled Population 
  disabled = rep(NA, n),
  
  # Residence 1 Year Ago
  
  # Same house
  same_house = rep(NA, n),
  # Different county in same state
  diff_county = rep(NA, n),
  # Different state
  diff_state = rep(NA, n),
  # Abroad
  abroad = rep(NA, n),
  
  # Employment Status
  
  # Labor force participation
  labor_force_participation = rep(NA, n),
  # Unemployment Rate
  unemployment = rep(NA, n),
  # Servicemen
  servicemen = rep(NA, n),
  
  # Commuting to Work
  
  # Car
  car = rep(NA, n),
  # Walking or Public Transit 
  walking_public_transit = rep(NA, n),
  # Worked from home
  worked_from_home = rep(NA, n),
  # Mean travel time to work (minutes)
  commute = rep(NA, n),
  
  # Class of Worker 
  
  # Private sector
  private_sector = rep(NA, n),
  # Public sector
  public_sector = rep(NA, n),
  # Self-employed
  self_employed = rep(NA, n),
  
  # Housing Occupancy 
  
  # Housing unit density (Total housing units / Land Area)
  # housing_density = rep(NA, n),
  # Homeowner vacancy rate
  homeowner_vacancy = rep(NA, n),
  # Rental vacancy rate
  rental_vacancy = rep(NA, n),
  
  # Housing Tenure
  
  # Owner occupation rate
  owner_occupied = rep(NA, n),
  # Rental occupation rate
  renter_occupied = rep(NA, n),
  
  # Year Householder Moved into Unit
  
  # 2 or less Years
  two_or_less = rep(NA, n),
  # 3-4 Years
  three_to_four = rep(NA, n),
  # 5-9 Years
  five_to_nine = rep(NA, n),
  # 10-19 Years
  ten_to_nineteen = rep(NA, n),
  # 20-29 Years
  twenty_to_twenty_nine = rep(NA, n),
  # 30 or more Years
  thirty_or_more = rep(NA, n),
  
  # Selected Monthly Owner Costs(SMOC)
  
  # Median SMOC Mortgage
  med_smoc_mort = rep(NA, n),
  # Median SMOC no Mortgage
  med_smoc_no_mort = rep(NA, n),
  
  # Gross Rent
  
  # Median Rent
  med_rent = rep(NA, n),
  
  # Income and Benefits
  
  # Median household income (dollars)
  med_hincome = rep(NA, n),
  # Mean household income (dollars)
  mean_hincome = rep(NA, n),
  
  # Health Insurance Coverage
  
  # Insured
  insured = rep(NA, n),
  
  # Private Health Insurance 
  private_coverage = rep(NA, n),
  
  # Public Health Insurance
  public_coverage = rep(NA, n),
  
  # Uninsured
  uninsured = rep(NA, n),
  
  # Percentage of Families and People Whose Income in the Past 12 Months is Below the Poverty Level
  
  # Families in Poverty
  pov_families = rep(NA, n),
  # Married Couple Families in Poverty
  pov_mcouples = rep(NA, n),
  # Single Mother Families
  pov_single_mothers = rep(NA, n),
  # People in Poverty
  pov = rep(NA, n),
  
  # Educational Attainment
  
  # No high school diploma
  no_hsdiploma = rep(NA, n),
  # High School Diploma
  high_school = rep(NA, n),
  # Some college, no degree
  some_college = rep(NA, n),
  # Associates
  associates = rep(NA, n),
  # Bachelors
  bachelors = rep(NA, n),
  # Graduate or professional degree
  graduate = rep(NA, n),
  
  stringsAsFactors = FALSE
)

rowMaker <- function(congressionalDistrict) {
  varRow <- list()
  for (
    v in colnames(districtDemographics)[1:(length(colnames(districtDemographics)) - 2)]
  ) {varRow[[v]] <- NA}
  
  singleDistrict <- wholeCountry %>% subset(district == congressionalDistrict)
  varRow$district <- singleDistrict %>% .$district %>% .[1]
  
  totalPop <- singleDistrict %>%
    filter(Title == "Total population") %>%
    .$statistic %>% .[1]
  
  # Sex and Age
  
  # sex_ratio
  # Sex Ratio (Male / Female) * 100
  varRow$sex_ratio <- (
    (singleDistrict %>% subset(Title == "Male") %>% .$statistic %>% .[1]) /
      (singleDistrict %>% subset(Title == "Female") %>% .$statistic %>% .[1])
  ) * 100
  
  # voting_age_pop
  # Voting Age Population (18 years and over)
  varRow$voting_age_pop <- singleDistrict %>%
    filter(Subject == "Sex and Age", Title == "18 years and over") %>%
    .$statistic %>% .[1]
  
  # minors
  # Minors (Total population - 18 years and over) * 100 / Total population
  varRow$minors <- ((totalPop - varRow$voting_age_pop) / (totalPop)) * 100
  
  # seniors
  # Seniors (65 years and older / Voting Age Population) * 100
  varRow$seniors <- (
    (singleDistrict %>%
       filter(Subject == "Sex and Age", Title == "65 years and over") %>%
       .$statistic %>% .[1]) /
      varRow$voting_age_pop
  ) * 100
  
  # med_age
  # Median age (years)
  varRow$med_age <- singleDistrict %>% filter(Title == "Median age (years)") %>%
    .$statistic %>% .[1] 
  
  # Race
  
  # white
  # Whites (White / Total population) * 100
  varRow$white <- (
    (singleDistrict %>% filter(Title == "White") %>% .$statistic %>% .[1]) /
      (totalPop)
    ) * 100
  
  # black
  # Blacks (Black or African American / Total population) * 100
  varRow$black <- (
    (singleDistrict %>% filter(Title == "Black or African American") %>% .$statistic %>% .[1]) /
      (totalPop)
  ) * 100
  
  # amerindian
  # American Indian (American Indian and Alaska Native / Total population) * 100
  varRow$amerindian <- (
    (singleDistrict %>% filter(Title == "American Indian and Alaska Native") %>% .$statistic %>% .[1]) /
      (totalPop)
  ) * 100
  
  # islander
  # Pacific Islander (Native Hawaiian and Other Pacific Islander / Total population) * 100
  varRow$islander <- (
    (singleDistrict %>% filter(Title == "Native Hawaiian and Other Pacific Islander") %>% .$statistic %>% .[1]) /
      (totalPop)
  ) * 100
  
  # asian
  # Asian (Asian / Total population) * 100
  varRow$asian <- (
    (singleDistrict %>% filter(Title == "Asian") %>% .$statistic %>% .[1]) /
      (totalPop)
  ) * 100 
  
  # other_race
  # Other (Some other race / Total population) * 100
  varRow$other_race <- (
    (singleDistrict %>% filter(Title == "Some other race") %>% .$statistic %>% .[1]) /
      (totalPop)
  ) * 100 
  
  # multiracial
  # Multiple Races (Two or more races / Total population) * 100
  varRow$multiracial <- (
    (singleDistrict %>% filter(Title == "Two or more races") %>% .$statistic %>% .[1]) /
      (totalPop)
  ) * 100
  
  # Hispanic or Latino and Race
  
  hispanicTotal <- singleDistrict %>%
    filter(Subject == "Hispanic or Latino and Race", Title == "Hispanic or Latino (of any race)") %>%
    .$statistic %>% .[1]
  
  # hispanic
  # Hispanic (Hispanic or Latino (of any race) / Total population) * 100
  varRow$hispanic <- (hispanicTotal / totalPop) * 100
  
  # mexican
  # Mexican (Mexican / Hispanic or Latino (of any race)) * 100
  varRow$mexican <- (
    (singleDistrict %>%
       filter(Subject == "Hispanic or Latino and Race", Title == "Mexican") %>%
       .$statistic %>% .[1]) /
      hispanicTotal
      ) * 100
  
  # puerto_rican
  # Puerto Rican (Puerto Rican / Hispanic or Latino (of any race)) * 100
  varRow$puerto_rican <- (
    (singleDistrict %>%
       filter(Subject == "Hispanic or Latino and Race", Title == "Puerto Rican") %>%
       .$statistic %>% .[1]) /
      hispanicTotal
  ) * 100
  
  # cuban
  # Cuban (Cuban / Hispanic or Latino (of any race)) * 100
  varRow$cuban <- (
    (singleDistrict %>%
       filter(Subject == "Hispanic or Latino and Race", Title == "Cuban") %>%
       .$statistic %>% .[1]) /
      hispanicTotal
  ) * 100
  
  # other_hispanic
  # Other Hispanic (Other Hispanic or Latino / Hispanic or Latino (of any race)) * 100
  varRow$other_hispanic <- (
    (singleDistrict %>%
       filter(Subject == "Hispanic or Latino and Race", Title == "Other Hispanic or Latino") %>%
       .$statistic %>% .[1]) /
      hispanicTotal
  ) * 100
  
  # natural_born_citizen
  # Natural Born Citizen (Native / Total Population) * 100
  varRow$natural_born_citizen <- (
    (singleDistrict %>%
       filter(Subject == "Place of Birth", Title == "Native") %>%
       .$statistic %>% .[1]) /
      totalPop
  ) * 100
  
  # born_in_state
  # Born in State of residence (State of residence / Native) * 100
  varRow$born_in_state <- (
    (singleDistrict %>%
       filter(Subject == "Place of Birth", Title == "State of residence") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Place of Birth", Title == "Native") %>%
         .$statistic %>% .[1])
  ) * 100
  
  # born_out_of_state
  # Born in Different state (Different state / Native) * 100
  varRow$born_out_of_state <- (
    (singleDistrict %>%
       filter(Subject == "Place of Birth", Title == "Different state") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Place of Birth", Title == "Native") %>%
         .$statistic %>% .[1])
  ) * 100
  
  # born_abroad
  # Born in US Territory, or born abroad to American parent(s)
  #   (Born in Puerto Rico, U.S. Island areas, or born abroad to American parent(s) / Native) * 100
  varRow$born_abroad <- (
    (singleDistrict %>%
       filter(
         Subject == "Place of Birth",
         Title == "Born in Puerto Rico, U.S. Island areas, or born abroad to American parent(s)"
         ) %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Place of Birth", Title == "Native") %>%
         .$statistic %>% .[1])
  ) * 100
  
  # foreign_born
  # Foreign born (Foreign born / Total Population)
  varRow$foreign_born <- (
    (singleDistrict %>%
       filter(Subject == "Place of Birth", Title == "Foreign born") %>%
       .$statistic %>% .[1]) /
      totalPop
  ) * 100
  
  # Disability Status of the Civilian Noninstitutionalized Population
  
  
  
  # disabled
  # (Voting Age) Disabled Population 
  #   (Total civilian noninstitutionalized population With a disability - Under 18 years With a disability) * 100 /
  #   (Total civilian noninstitutionalized population)
  varRow$disabled <- (
    ((singleDistrict %>%
        filter(
          Subject == "Disability Status of the Civilian Noninstitutionalized Population",
          Title == "With a disability"
        ) %>% .$statistic %>% .[1]) -
       (singleDistrict %>%
          filter(
            Subject == "Disability Status of the Civilian Noninstitutionalized Population",
            Title == "With a disability"
          ) %>% .$statistic %>% .[2])) /
      (singleDistrict %>%
         filter(
           Subject == "Disability Status of the Civilian Noninstitutionalized Population",
           Title == "Total civilian noninstitutionalized population"
         ) %>% .$statistic %>% .[1])
  ) * 100
  
  # Residence 1 Year Ago
  
  over1 <- singleDistrict %>%
    filter(Subject == "Residence 1 Year Ago", Title == "Population 1 year and over") %>%
    .$statistic %>% .[1]
  
  # same_house
  # Same house (Same house / Population 1 year and over) * 100
  varRow$same_house <- (
    (singleDistrict %>%
       filter(Subject == "Residence 1 Year Ago", Title == "Same house") %>%
       .$statistic %>% .[1]) /
      over1
  ) * 100
  
  # diff_county
  # Different county in same state (Different County / Population 1 year and over) * 100
  varRow$diff_county <- (
    (singleDistrict %>%
       filter(Subject == "Residence 1 Year Ago", Title == "Different county") %>%
       .$statistic %>% .[1]) /
      over1
  ) * 100
  
  # diff_state
  # Different state (Different state / Population 1 year and over) * 100
  varRow$diff_state <- (
    (singleDistrict %>%
       filter(Subject == "Residence 1 Year Ago", Title == "Different state") %>%
       .$statistic %>% .[1]) /
      over1
  ) * 100
  
  
  # abroad
  # Abroad (Abroad / Population 1 year and over) * 100
  varRow$abroad <- (
    (singleDistrict %>%
       filter(Subject == "Residence 1 Year Ago", Title == "Abroad") %>%
       .$statistic %>% .[1]) /
      over1
  ) * 100
  
  # Employment Status
  
  over16 <- singleDistrict %>%
    filter(Subject == "Employment Status", Title == "Population 16 years and over") %>%
    .$statistic %>% .[1]
  
  # labor_force_participation
  # Labor force participation (In labor force / Population 16 years and over) * 100
  varRow$labor_force_participation <- (
    (singleDistrict %>%
       filter(Subject == "Employment Status", Title == "In labor force") %>%
       .$statistic %>% .[1]) /
      over16
  ) * 100
  
  # unemployment
  # Unemployment Rate
  varRow$unemployment <- singleDistrict %>%
    filter(Subject == "Employment Status", Title == "Unemployment Rate") %>%
    .$statistic %>% .[1]
  
  
  # servicemen
  # Servicemen (Armed Forces / Population 16 years and over) * 100
  varRow$servicemen <- (
    (singleDistrict %>%
       filter(Subject == "Employment Status", Title == "Armed Forces") %>%
       .$statistic %>% .[1]) /
      over16
  ) * 100
  
  # Commuting to Work
  
  workers_over_16 <- singleDistrict %>%
    filter(Title == "Workers 16 years and over") %>%
    .$statistic %>% .[1]
  
  # car
  # Car
  #  (Car, truck, or van -- drove alone + Car, truck, or van -- carpooled) * 100 /
  #    Workers 16 years and over
  varRow$car <- (((singleDistrict %>%
    filter(Subject == "Commuting to Work", Title == "Car, truck, or van -- drove alone") %>%
    .$statistic %>% .[1]) +
    (singleDistrict %>%
       filter(Subject == "Commuting to Work", Title == "Car, truck, or van -- carpooled") %>%
       .$statistic %>% .[1])) /
    workers_over_16) * 100
    
  # walking_public_transit
  # Walking or Public Transit 
  #  (Public transportation (excluding taxicab) + Walked) * 100 / Workers 16 years and over
  varRow$walking_public_transit <- (((singleDistrict %>%
    filter(Subject == "Commuting to Work", Title == "Public transportation (excluding taxicab)") %>%
    .$statistic %>% .[1]) +
    (singleDistrict %>%
       filter(Subject == "Commuting to Work", Title == "Walked") %>%
       .$statistic %>% .[1])) /
      workers_over_16) * 100
  
  # worked_from_home
  # Worked from home (Worked from home / Workers 16 years and over) * 100
  varRow$worked_from_home <- (
    (singleDistrict %>%
       filter(Subject == "Commuting to Work", Title == "Worked from home") %>%
       .$statistic %>% .[1]) /
      workers_over_16
  ) * 100
  
  
  # commute
  # Mean travel time to work (minutes)
  varRow$commute <- singleDistrict %>%
    filter(Subject == "Commuting to Work", Title == "Mean travel time to work (minutes)") %>%
    .$statistic %>% .[1]
  
  # Class of Worker
  
  civilian_workers_over_16 <- singleDistrict %>%
    filter(Subject == "Class of Worker", Title == "Civilian employed population 16 years and over") %>%
    .$statistic %>% .[1]
  
  # private_sector
  # Private sector (Private wage and salary worker / Civilian employed population 16 years and over) * 100
  varRow$private_sector <- (
    (singleDistrict %>%
      filter(Subject == "Class of Worker", Title == "Private wage and salary workers") %>%
       .$statistic %>% .[1]) /
      civilian_workers_over_16
  ) * 100
  
  # public_sector
  # Public sector (Government workers / Civilian employed population 16 years and over) * 100
  varRow$public_sector <- (
    (singleDistrict %>%
       filter(Subject == "Class of Worker", Title == "Government workers") %>%
       .$statistic %>% .[1]) /
      civilian_workers_over_16
  ) * 100
  
  # self_employed
  # Self-employed 
  #  (Self-employed in own not incorporated business workers / Civilian employed population 16 years and over) * 100
  varRow$self_employed <- (
    (singleDistrict %>%
       filter(Subject == "Class of Worker", Title == "Self-employed in own not incorporated business workers") %>%
       .$statistic %>% .[1]) /
      civilian_workers_over_16
  ) * 100
  
  # Housing Occupancy 
  
  # homeowner_vacancy
  # Homeowner vacancy rate
  varRow$homeowner_vacancy <- singleDistrict %>%
    filter(Subject == "Housing Occupancy", Title == "Homeowner vacancy rate") %>%
    .$statistic %>% .[1]
  
  # rental_vacancy
  # Rental vacancy rate
  varRow$rental_vacancy <- singleDistrict %>%
    filter(Subject == "Housing Occupancy", Title == "Rental vacancy rate") %>%
    .$statistic %>% .[1]
  
  
  # Housing Tenure
  
  occupiedHousing <- singleDistrict %>%
    filter(Title == "Occupied housing units") %>%
    .$statistic %>% .[1]
  
  # owner_occupied
  # Owner occupation rate (Owner-occupied / Occupied housing units) * 100
  varRow$owner_occupied <- (
    (singleDistrict %>%
       filter(Subject == "Housing Tenure", Title == "Owner-occupied") %>%
       .$statistic %>% .[1]) /
      occupiedHousing
  ) * 100
  
  # renter_occupied
  # Rental occupation rate (Renter-occupied / Occupied housing units) * 100
  varRow$renter_occupied <- (
    (singleDistrict %>%
       filter(Subject == "Housing Tenure", Title == "Renter-occupied") %>%
       .$statistic %>% .[1]) /
      occupiedHousing
  ) * 100
  
  # Year Householder Moved into Unit
  
  # two_or_less
  # 2 or less Years (Moved in 2017 or later / Occupied housing units)
  varRow$two_or_less <- (
    (singleDistrict %>%
       filter(Title == "Moved in 2017 or later") %>%
       .$statistic %>% .[1]) /
      occupiedHousing
  ) * 100
  
  # three_to_four
  # 3-4 Years (Moved in 2015 to 2016 / Occupied housing units)
  varRow$three_to_four <- (
    (singleDistrict %>%
       filter(Title == "Moved in 2015 to 2016") %>%
       .$statistic %>% .[1]) /
      occupiedHousing
  ) * 100
  
  # five_to_nine
  # 5-9 Years (Moved in 2014 to 2014  / Occupied housing units)
  varRow$five_to_nine <- (
    (singleDistrict %>%
       filter(Title == "Moved in 2014 to 2014") %>%
       .$statistic %>% .[1]) /
      occupiedHousing
  ) * 100
  
  # ten_to_nineteen
  # 10-19 Years (Moved in 2000 to 2009 / Occupied housing units)
  varRow$ten_to_nineteen <- (
    (singleDistrict %>%
       filter(Title == "Moved in 2000 to 2009") %>%
       .$statistic %>% .[1]) /
      occupiedHousing
  ) * 100
  
  
  # twenty_to_twenty_nine
  # 20-29 Years (Moved in 1990 to 1999 / Occupied housing units)
  varRow$twenty_to_twenty_nine <- (
    (singleDistrict %>%
       filter(Title == "Moved in 1990 to 1999") %>%
       .$statistic %>% .[1]) /
      occupiedHousing
  ) * 100
  
  # thirty_or_more
  # 30 or more Years (Moved in 1989 and earlier / Occupied housing units)
  varRow$thirty_or_more <- (
    (singleDistrict %>%
       filter(Title == "Moved in 1989 and earlier") %>%
       .$statistic %>% .[1]) /
      occupiedHousing
  ) * 100
  
  # Selected Monthly Owner Costs(SMOC)
  
  # med_smoc_mort
  # Median SMOC Mortgage (Median (dollars))
  varRow$med_smoc_mort <- singleDistrict %>%
    filter(Subject == "Selected Monthly Owner Costs(SMOC)", Title == "Median (dollars)") %>%
    .$statistic %>% .[1]
  
  
  # med_smoc_no_mort
  # Median SMOC no Mortgage (Median (dollars))
  varRow$med_smoc_no_mort <- singleDistrict %>%
    filter(Subject == "Selected Monthly Owner Costs(SMOC)", Title == "Median (dollars)") %>%
    .$statistic %>% .[2]
  
  # Gross Rent
  
  # med_rent
  # Median Rent (Median (dollars))
  varRow$med_rent <- singleDistrict %>%
    filter(Subject == "Gross Rent", Title == "Median (dollars)") %>%
    .$statistic %>% .[1]
  
  # Income and Benefits (In 2019 inflation-adjusted dollars)
  
  # med_hincome
  # Median household income (dollars)
  varRow$med_hincome <- singleDistrict %>%
    filter(
      Subject == "Income and Benefits (In 2018 inflation-adjusted dollars)",
      Title == "Median household income (dollars)"
      ) %>%
    .$statistic %>% .[1]
  
  # mean_hincome
  # Mean household income (dollars)
  varRow$mean_hincome <- singleDistrict %>%
    filter(
      Subject == "Income and Benefits (In 2018 inflation-adjusted dollars)",
      Title == "Mean household income (dollars)"
    ) %>%
    .$statistic %>% .[1]
  
  # Health Insurance Coverage
  
  cnp <- singleDistrict %>%
    filter(Title == "Civilian noninstitutionalized population") %>%
    .$statistic %>% .[1]
  
  cnpCovered <- singleDistrict %>%
    filter(Title == "With health insurance coverage") %>%
    .$statistic %>% .[1]
    
  
  # insured
  # Insured
  #  (Civilian noninstitutionalized population With health insurance coverage /
  #   Civilian noninstitutionalized population) * 100
  varRow$insured <- (cnpCovered / cnp) * 100
  
  # private_coverage
  # Private Health Insurance 
  #  (Civilian noninstitutionalized population With private health insurance /
  #   Civilian noninstitutionalized population With health insurance coverage) * 100
  varRow$private_coverage <- (
    (singleDistrict %>%
       filter(Title == "With private health insurance") %>%
      .$statistic %>% .[1]) /
      cnpCovered
  ) * 100
  
  # public_coverage
  # Public Health Insurance
  #  (Civilian noninstitutionalized population With public coverage /
  #   Civilian noninstitutionalized population With health insurance coverage) * 100
  varRow$public_coverage <- (
    (singleDistrict %>%
       filter(Title == "With public coverage") %>%
       .$statistic %>% .[1]) /
      cnpCovered
  ) * 100
  
  # uninsured
  # Uninsured
  #  (Civilian noninstitutionalized population No health insurance coverage /
  #   Civilian noninstitutionalized population) * 100
  varRow$uninsured <- (
    (singleDistrict %>%
       filter(Subject == "Health Insurance Coverage") %>%
       .$statistic %>% .[5]) /
      cnp
  ) * 100
  
  # Percentage of Families and People Whose Income in the Past 12 Months is Below the Poverty Level
  povString <- "Percentage of Families and People Whose Income in the Past 12 Months is Below the Poverty Level"
  
  # pov_families
  # Families in Poverty (All Families)
  varRow$pov_families <- singleDistrict %>%
    filter(Subject == povString, Title == "All families") %>%
    .$statistic %>% .[1]
  
  # pov_mcouples
  # Married Couple Families in Poverty (Married couple families)
  varRow$pov_mcouples <- singleDistrict %>%
    filter(Subject == povString, Title == "Married couple families") %>%
    .$statistic %>% .[1]
  
  # pov_single_mothers
  # Single Mother Families (Families with female householder, no spouse present)
  varRow$pov_single_mothers <- singleDistrict %>%
    filter(Subject == povString, Title == "Families with female householder, no spouse present") %>%
    .$statistic %>% .[1]
  
  # pov
  # People in Poverty (All people)
  varRow$pov <- singleDistrict %>%
    filter(Subject == povString, Title == "All people") %>%
    .$statistic %>% .[1]
  
  # Educational Attainment
  
  over25 <- singleDistrict %>%
    filter(Title == "Population 25 years and over") %>%
    .$statistic %>% .[1]
  
  # no_hsdiploma
  # No high school diploma
  #  ((Less than 9th grade + 9th to 12th grade, no diploma) / Population 25 years and over) * 100
  varRow$no_hsdiploma <- (
    ((singleDistrict %>%
       filter(Title == "Less than 9th grade") %>%
       .$statistic %>% .[1]) +
      (singleDistrict %>%
         filter(Title == "9th to 12th grade, no diploma") %>%
         .$statistic %>% .[1])) /
      over25  
  ) * 100
  
  # high_school
  # High School Diploma (High school graduate (includes equivalency) / Population 25 years and over) * 100
  varRow$high_school <- (
    (singleDistrict %>%
       filter(Title == "High school graduate (includes equivalency)") %>%
       .$statistic %>% .[1]) /
      over25
  ) * 100
  
  # some_college
  # Some college, no degree (Some college, no degree / Population 25 years and over) * 100
  varRow$some_college <- (
    (singleDistrict %>%
       filter(Title == "Some college, no degree") %>%
       .$statistic %>% .[1]) /
      over25
  ) * 100
  
  # associates
  # Associates (Associate's degree / Population 25 years and over) * 100
  varRow$associates <- (
    (singleDistrict %>%
       filter(Title == "Associate's degree") %>%
       .$statistic %>% .[1]) /
      over25
  ) * 100
  
  # bachelors
  # Bachelors (Bachelor's degree / Population 25 years and over) * 100
  varRow$bachelors <- (
    (singleDistrict %>%
       filter(Title == "Bachelor's degree") %>%
       .$statistic %>% .[1]) /
      over25
  ) * 100
  
  # graduate
  # Graduate or professional degree (Graduate or professional degree / Population 25 years and over) * 100
  varRow$graduate <- (
    (singleDistrict %>%
       filter(Title == "Graduate or professional degree") %>%
       .$statistic %>% .[1]) /
      over25
  ) * 100
  
  return(varRow)
}

for (i in 1:length(unique(wholeCountry$district))) {
  for (
    v in colnames(districtDemographics)[1:(length(colnames(districtDemographics)))]
  ) {
    print(glue::glue("{unique(wholeCountry$district)[i]}: {v}"))
    newRow <- rowMaker(unique(wholeCountry$district)[i])
    districtDemographics[i, v] <- newRow[[v]]
  }
}

hr$districtDemographics <- paste(hr$state, hr$district)

for (i in 1:length(hr$districtDemographics)) {
  hr$districtDemographics[i] <- gsub(
    "Congressional District ", "", hr$districtDemographics[i]
    )
  if (hr$state[i] %in% atLarge) {
    hr$districtDemographics[i] <- gsub(
      "1", "At Large", hr$districtDemographics[i]
    )
  }
}

colnames(districtDemographics)[1] <- "districtDemographics"

mergehr <- hr[, c("state", "district", "party", "flipped")]

for (i in 1:length(mergehr$district)) {
  mergehr$district[i] <- strsplit(mergehr$district[i], split = " ")[[1]][3]
}

mergehr$districtDemographics <- paste(mergehr$state, mergehr$district)
mergehr[, c("districtDemographics", "party", "flipped")]

colnames(districtDemographics[1]) <- "districtDemographics"
districtDemographics <- merge(
  districtDemographics, mergehr, by = "districtDemographics"
  )