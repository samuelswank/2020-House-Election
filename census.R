library(tidyverse)

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
    # Accounting for Washington DC
    } else if (strsplit(f, split = "_")[[1]][1] == "District") {
      atLarge <- atLarge %>% append(
        paste(
          strsplit(f, split = "_")[[1]][1],
          strsplit(f, split = "_")[[1]][2],
          strsplit(f, split = "_")[[1]][3]
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

statesDC <- state.name %>% append("District of Columbia")
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
  
  # Sex Ratio (Male / Female) * 1000
  sex_ratio = rep(NA, n),
  # Minors (Total population - 18 years and over) * 100 / (Total population)
  minors = rep(NA, n),
  # Voting Age Population (18 years and over)
  voting_age_pop = rep(NA, n),
  # Seniors (65 years and older / Voting Age Population) * 100
  seniors = rep(NA, n),
  # Median age (years)
  med_age = rep(NA, n),
  
  # Race
  
  # Whites (White / Total population) * 100
  white = rep(NA, n),
  # Blacks (Black or African American / Total population) * 100
  black = rep(NA, n),
  # American Indian (American Indian and Alaska Native / Total population) * 100
  amerindian = rep(NA, n),
  # Pacific Islander (Native Hawaiian and Other Pacific Islander / Total population) * 100
  islander = rep(NA, n), 
  # Other (Some other race / Total population) * 100
  other_race = rep(NA, n), 
  # Multiple Races (Two or more races / Total population) * 100
  multiracial = rep(NA, n),
  
  # Hispanic or Latino and Race
  
  # Hispanic (Hispanic or Latino (of any race) / Total population) * 100
  hispanic = rep(NA, n),
  # Mexican (Mexican / Hispanic or Latino (of any race)) * 100
  mexican = rep(NA, n),
  # Puerto Rican (Puerto Rican / Hispanic or Latino (of any race)) * 100
  puerto_rican = rep(NA, n),
  # Cuban (Cuban / Hispanic or Latino (of any race)) * 100
  cuban = rep(NA, n),
  # Other Hispanic (Other Hispanic or Latino / Hispanic or Latino (of any race)) * 100
  other_hispanic = rep(NA, n),
  # Natural born citizen (Native / Total Population) * 100
  
  # Place of Birth
  
  # Natural Born Citizen
  natural_born_citizen = rep(NA, n),
  # Born in State of residence (State of residence / Native) * 100
  born_in_state = rep(NA, n),
  # Born in Different state (Different state / Native) * 100
  born_out_of_state = rep(NA, n),
  # Born in US Territory, or born abroat to American parent(s)
  #   (Born in Puerto Rico, U.S. Island areas, or born abroad to American parent(s) / Native) * 100
  born_abroad = rep(NA, n),
  # Foreign Born (Foreign born / Total Population)
  foreign_born = rep(NA, n),
  # Disability Status of the Civilian Noninstitutionalized Population
  
  # (Voting Age) Disabled Population 
  #   (Total civilian noninstitutionalized population With a disability - Under 18 years With a disability) * 100 /
  #   (Total civilian noninstitutionalized population)
  disabled = rep(NA, n),

  # Residence 1 Year Ago
  
  # Same house (Same house / Population 1 year and over) * 100
  same_house = rep(NA, n),
  # Different county in same state (Different County / Population 1 year and over) * 100
  diff_county = rep(NA, n),
  # Different state (Different state / Population 1 year and over) * 100
  diff_state = rep(NA, n),
  # Abroad (Abroad / Population 1 year and over) * 100
  abroad = rep(NA, n),
  
  # Employment Status
  
  # Labor force participation (In labor force / Population 16 years and over) * 100
  labor_force_participation = rep(NA, n),
  # Unemployment Rate
  unemployment = rep(NA, n),
  # Servicemen (Armed Forces / Population 16 years and over) * 100
  servicemen = rep(NA, n),
  
  # Commuting to Work
  
  # Car
  #  (Car, truck, or van -- drove alone + Car, truck, or van -- carpooled) * 100 /
  #    Workers 16 years and over
  car = rep(NA, n),
  # Walking or Public Transit 
  #  (Public transportation (excluding taxicab) + Walked) * 100 / Workers 16 years and over
  walking_public_transit = rep(NA, n),
  # Worked from home (Worked from home / Workers 16 years and over) * 100
  worked_from_home = rep(NA, n),
  # Mean travel time to work (minutes)
  commute = rep(NA, n),
  
  # Class of Worker 
  
  # Private sector (Private wage and salary worker / Civilian employed population 16 years and over) * 100
  private_sector = rep(NA, n),
  # Public sector (Government workers / Civilian employed population 16 years and over) * 100
  public_sector = rep(NA, n),
  # Self-employed (Self-employed in own not incorporated business workers / Civilian employed population 16 years and over) * 100
  self_employed = rep(NA, n),
  
  # Housing Occupancy 
  
  # Housing unit density (Total housing units / Land Area)
  # housing_density = rep(NA, n),
  # Homeowner vacancy rate
  homeowner_vacancy = rep(NA, n),
  # Rental vacancy rate
  rental_vacancy = rep(NA, n),
  
  # Housing Tenure
  
  # Owner occupation rate (Owner-occupied / Occupied housing units) * 100
  owner_occupied = rep(NA, n),
  # Rental occupation rate (Renter-occupied / Occupied housing units) * 100
  renter_occupied = rep(NA, n),
  
  # Year Householder Moved into Unit
  
  # 2 or less Years (Moved in 2017 or later / Occupied housing units)
  two_or_less = rep(NA, n),
  # 3-4 Years (Moved in 2015 to 2016 / Occupied housing units)
  three_to_four = rep(NA, n),
  # 5-9 Years (Moved in 2014 to 2014  / Occupied housing units)
  five_to_nine = rep(NA, n),
  # 10-19 Years (Moved in 2000 to 2009 / Occupied housing units)
  ten_to_nineteen = rep(NA, n),
  # 20-29 Years (Moved in 1990 to 1999 / Occupied housing units)
  twenty_to_twenty_nine = rep(NA, n),
  # 30 or more Years (Moved in 1989 and earlier / Occupied housing units)
  thirty_or_more = rep(NA, n),
  
  # Selected Monthly Owner Costs(SMOC)
  
  # Median SMOC Mortgage (Median (dollars))
  med_smoc_mort = rep(NA, n),
  # Median SMOC no Mortgage (Median (dollars))
  med_smoc_no_mort = rep(NA, n),
  
  # Gross Rent
  
  # Median Rent (Median (dollars))
  med_rent = rep(NA, n),
  
  # Income and Benefits (In 2019 inflation-adjusted dollars)
  
  # Median household income (dollars)
  med_hincome = rep(NA, n),
  # Mean household income (dollars)
  mean_hincome = rep(NA, n),
  
  # Health Insurance Coverage
  
  # - Insured
  #    (Civilian noninstitutionalized population With health insurance coverage /
  #     Civilian noninstitutionalized population) * 100
  insured = rep(NA, n),
  
  # - Private Health Insurance 
  #    (Civilian noninstitutionalized population With private health insurance /
  #     Civilian noninstitutionalized population With health insurance coverage) * 100
  private_coverage = rep(NA, n),
  
  # - Public Health Insurance
  #    (Civilian noninstitutionalized population With public coverage /
  #     Civilian noninstitutionalized population With health insurance coverage) * 100
  
  # - Uninsured
  #    (Civilian noninstitutionalized population No health insurance coverage /
  #     Civilian noninstitutionalized population) * 100
  
  public_coverage = rep(NA, n),
  
  # Percentage of Families and People Whose Income in the Past 12 Months is Below the Poverty Level
  
  # Families in Poverty (All Families)
  pov_families = rep(NA, n),
  # Married Couple Families in Poverty (Married couple families)
  pov_mcouples = rep(NA, n),
  # Single Mother Families (Families with female householder, no spouse present)
  pov_single_mothers = rep(NA, n),
  # People in Poverty (All people)
  pov = rep(NA, n),
  
  # Educational Attainment
  
  # - No high school diploma
  #    ((Less than 9th grade + 9th to 12th grade, no diploma) / Population 25 years and over) * 100
  no_hsdiploma = rep(NA, n),
  # - High School Diploma (High school graduate (includes equivalency) / Population 25 years and over) * 100
  high_school = rep(NA, n),
  # - Some college, no degree (Some college, no degree / Population 25 years and over) * 100
  some_college = rep(NA, n),
  # - Associates (Associate's degree / Population 25 years and over) * 100
  associates = rep(NA, n),
  # - Bachelors (Bachelor's degree / Population 25 years and over) * 100
  bachelors = rep(NA, n),
  # - Graduate or professional degree (Graduate or professional degree / Population 25 years and over) * 100
  graduate = rep(NA, n),
  
  party = rep(NA, n),
  flipped = rep(NA, n),
  
  stringsAsFactors = FALSE
  )

rowMaker <- function(congressionalDistrict) {
  varRow <- list()
  for (
    v in colnames(districtDemographics)[1:length(colnames(districtDemographics))]
  ) {varRow[[v]] <- NA}
  
  singleDistrict <- wholeCountry %>% subset(district == congressionalDistrict)
  
  varRow$district <- singleDistrict %>% .$district %>% .[1]
  
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
  # Minors (Total population - 18 years and over) * 100 / (Total population)
  varRow$minors <- (
    (singleDistrict %>%
       subset(Subject = "Sex and Age", Title = "Total Population") %>%
       .$statistic %>% .[1]) -
      varRow$voting_age_pop) /
    (singleDistrict %>%
       subset(Subject = "Sex and Age", Title = "Total Population") %>%
       .$statistic %>% .[1]) * 100
  
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
    .$statistic %>%
    .[1] 
  
  # Race
  
  # white
  # Whites (White / Total population) * 100
  varRow$white <- (
    (singleDistrict %>% filter(Title == "White") %>% .$statistic %>% .[1]) /
      (singleDistrict %>% filter(Subject == "Race", Title == "Total population")
       %>% .$statistic %>% .[1])
    ) * 100
  
  # black
  # Blacks (Black or African American / Total population) * 100
  varRow$black <- (
    (singleDistrict %>%
       filter(Title == "Black or African American") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>% filter(Subject == "Race", Title == "Total population")
       %>% .$statistic %>% .[1])
    ) * 100
  
  # amerindian
  # American Indian (American Indian and Alaska Native / Total population) * 100
  varRow$amerindian <- (
    (singleDistrict %>%
       filter(Title == "American Indian and Alaska Native") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Race", Title == "Total population") %>%
         .$statistic %>% .[1])
    ) * 100 
  
  # islander
  # Pacific Islander (Native Hawaiian and Other Pacific Islander / Total population) * 100
  varRow$islander <- (
    (singleDistrict %>%
       filter(Title == "Native Hawaiian and Other Pacific Islander") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>% filter(Subject == "Race", Title == "Total population") %>%
         .$statistic %>% .[1])
    ) * 100 
  
  # other_race
  # Other (Some other race / Total population) * 100
  varRow$other_race <- (
    (singleDistrict %>%
       filter(Title == "Some other race") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Race", Title == "Total population") %>%
         .$statistic %>% .[1])
    ) * 100 
  
  # multiracial
  # Multiple Races (Two or more races / Total population) * 100
  varRow$multiracial <- (
    (singleDistrict %>%
       filter(Title == "Two or more races") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Race", Title == "Total population") %>%
         .$statistic %>% .[1])
    ) * 100 
  
  # Hispanic or Latino and Race
  
  # hispanic
  # Hispanic (Hispanic or Latino (of any race) / Total population) * 100
  varRow$hispanic <- (
    (singleDistrict %>%
       filter(Title == "Hispanic or Latino (of any race)") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Hispanic or Latino and Race", Title == "Total population") %>%
         .$statistic %>% .[1])
    ) * 100
  
  # mexican
  # Mexican (Mexican / Hispanic or Latino (of any race)) * 100
  varRow$mexican <- (
    (singleDistrict %>%
       filter(Subject == "Hispanic or Latino and Race", Title == "Mexican") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Title == "Hispanic or Latino (of any race)") %>%
         .$statistic %>% .[1])) * 100
  
  # puerto_rican
  # Puerto Rican (Puerto Rican / Hispanic or Latino (of any race)) * 100
  varRow$puerto_rican <- (
    (singleDistrict %>%
       filter(Subject == "Hispanic or Latino and Race", Title == "Puerto Rican") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Title == "Hispanic or Latino (of any race)") %>%
         .$statistic %>% .[1])
    ) * 100
  
  # cuban
  # Cuban (Cuban / Hispanic or Latino (of any race)) * 100
  varRow$cuban <- (
    (singleDistrict %>%
       filter(Subject == "Hispanic or Latino and Race", Title == "Cuban") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Title == "Hispanic or Latino (of any race)") %>%
         .$statistic %>% .[1])
    ) * 100
  
  # other_hispanic
  # Other Hispanic (Other Hispanic or Latino / Hispanic or Latino (of any race)) * 100
  varRow$other_hispanic <- (
    (singleDistrict %>%
       filter(Subject == "Hispanic or Latino and Race", Title == "Other Hispanic or Latino") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Title == "Hispanic or Latino (of any race)") %>%
         .$statistic %>% .[1])) * 100
  
  # natural_born_citizen
  # Natural Born Citizen (Native / Total Population) * 100
  varRow$natural_born_citizen <- (
    (singleDistrict %>%
       filter(Subject == "Place of Birth", Title == "Native") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Place of Birth", Title == "Total population") %>%
         .$statistic %>% .[1])
  ) * 100
  
  # born_in_state
  # Born in State of residence (State of residence / Native) * 100
  varRow$born_in_state <- (
    (singleDistrict %>%
       filter(Subject == "Place of Birth", Title == "State of residence") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Place of Birth", Title == "Total population") %>%
         .$statistic %>% .[1])
  ) * 100
  
  # born_out_of_state
  # Born in Different state (Different state / Native) * 100
  varRow$born_out_of_state <- (
    (singleDistrict %>%
       filter(Subject == "Place of Birth", Title == "Different state") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Place of Birth", Title == "Total population") %>%
         .$statistic %>% .[1])
  ) * 100
  
  # born_abroad
  # Born in US Territory, or born abroad to American parent(s)
  #   (Born in Puerto Rico, U.S. Island areas, or born abroad to American parent(s) / Native) * 100
  varRow$born_abroad <- (
    (singleDistrict %>%
       filter(Subject == "Place of Birth", Title == "Born in Puerto Rico, U.S. Island areas, or born abroad to American parent(s)") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Place of Birth", Title == "Total population") %>%
         .$statistic %>% .[1])
  ) * 100
  
  # foreign_born
  # Foreign born (Foreign born / Total Population)
  varRow$foreign_born <- (
    (singleDistrict %>%
       filter(Subject == "Place of Birth", Title == "Foreign born") %>%
       .$statistic %>% .[1]) /
      (singleDistrict %>%
         filter(Subject == "Place of Birth", Title == "Total population") %>%
         .$statistic %>% .[1])
  ) * 100
  
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
  
  return(varRow)
}






