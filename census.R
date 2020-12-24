library(tidyverse)

wholeState <- read_csv("data/census/Alabama_District_all.csv")[
  , 2:length(colnames(read_csv("data/census/Alabama_District_all.csv")))
  ]

for (col in wholeState %>% select(ends_with("Estimate")) %>% colnames()) {
  wholeState[, col] <- wholeState[, col] %>% lapply(function(x) as.numeric(as.character(x)))
} 

for (col in wholeState %>% select(ends_with("MOE")) %>% colnames()) {
  wholeState[, col] <- wholeState[, col] %>% 
    lapply(function(x) abs(as.numeric(gsub("[^0-9.-]", "", x))))
}

wholeState <- wholeState %>% 
  gather(district, statistic, 3:length(colnames(wholeState)))

wholeState <- wholeState %>% separate(district, c("state", "district", "type"))

for (i in 1:length(wholeState$state)) {wholeState[i, "state"] <- "Alabama"}

wholeState$district <- as.numeric(wholeState$district)

categories <- list()

for (category in unique(wholeState$Subject)) {
  subjectSubset <- wholeState %>% filter(Subject == category)
  categories[[category]] <- subjectSubset
}

# subsets outputting list columns from pivot_wider
  # Disability Status of the Civilian Noninstitutionalized Population <- cdTopic: People
  # Employment Status <- cdTopic: Workers
  # Selected Monthly Owner Costs(SMOC) <- cdTopic: Housing
  # Health Insurance Coverage <- cdTopic: Socioeconomic
  # Percentage of Families and People Whose Income in the Past 12 Months is Below the Poverty Level <- cdTopic: Socioeconomic

# Adressing Duplicate Titles

# Disability Status of the Civilian Noninstitutionalized Population
for (i in 1:length(categories$`Disability Status of the Civilian Noninstitutionalized Population`$Title)) {
  if (categories$`Disability Status of the Civilian Noninstitutionalized Population`$Title[i] == "With a disability") {
    categories$`Disability Status of the Civilian Noninstitutionalized Population`$Title[i] <- paste(
      categories$`Disability Status of the Civilian Noninstitutionalized Population`$Title[i - 1],
      categories$`Disability Status of the Civilian Noninstitutionalized Population`$Title[i]
    )
  }
}

# Employment Status
# Rows containing superfluous data to be removed
tbr <- c()
for (i in 1:length(categories$`Employment Status`$Title)) {
  if (
    categories$`Employment Status`$Title[i] == "Civilian labor force" &
    categories$`Employment Status`$Title[i + 1] == "Unemployment Rate"
  ) {tbr <- tbr %>% append(i)}
}

categories$`Employment Status` <- categories$`Employment Status`[-tbr, ]

# Selected Monthly Owner Costs(SMOC)

huwom <- categories$`Selected Monthly Owner Costs(SMOC)`$Title[2:8]
huwm  <- categories$`Selected Monthly Owner Costs(SMOC)`$Title[11:16]

for (i in 2:length(categories$`Selected Monthly Owner Costs(SMOC)`$Title)) {
  if (
    categories$`Selected Monthly Owner Costs(SMOC)`$Title[i] %in% huwom |
    categories$`Selected Monthly Owner Costs(SMOC)`$Title[i - 1] == "Housing units with a mortgage $3,000 or more" 
    ) {
    categories$`Selected Monthly Owner Costs(SMOC)`$Title[i] <- paste(
      categories$`Selected Monthly Owner Costs(SMOC)`$Title[1],
      categories$`Selected Monthly Owner Costs(SMOC)`$Title[i]
    )
  } else if (
    categories$`Selected Monthly Owner Costs(SMOC)`$Title[i] %in% huwm |
    categories$`Selected Monthly Owner Costs(SMOC)`$Title[i - 1] == "Housing units without a mortgage $1,000 or more" 
  ) {
    categories$`Selected Monthly Owner Costs(SMOC)`$Title[i] <- paste(
      categories$`Selected Monthly Owner Costs(SMOC)`$Title[10],
      categories$`Selected Monthly Owner Costs(SMOC)`$Title[i]
    )
  }
}

# Health Insurance Coverage

cnip <- categories$`Health Insurance Coverage`$Title[2:5]

for (i in 2:length(categories$`Health Insurance Coverage`$Title)) {
  if (
    categories$`Health Insurance Coverage`$Title[i] %in% cnip &
    categories$`Health Insurance Coverage`$Title[i - 1] != "Civilian noninstitutionalized population under 19 years"
    ) {
    categories$`Health Insurance Coverage`$Title[i] <- paste(
      categories$`Health Insurance Coverage`$Title[1],
      categories$`Health Insurance Coverage`$Title[i]
    )
  } else if (categories$`Health Insurance Coverage`$Title[i - 1] == "Civilian noninstitutionalized population under 19 years") {
    categories$`Health Insurance Coverage`$Title[i] <- paste(
      categories$`Health Insurance Coverage`$Title[i - 1],
      categories$`Health Insurance Coverage`$Title[i]
    )
  }
}

# Percentage of Families and People Whose Income in the Past 12 Months is Below the Poverty Level

for (i in 1:length(categories[[23]]$Title)) {
  if (categories[[23]]$Title[i] == "With related children of the householder under 18 years") {
    categories[[23]]$Title[i] <- paste(
      categories[[23]]$Title[i - 1], categories[[23]]$Title[i]
      )
  } else if (categories[[23]]$Title[i] == "With related children of the householder under 5 years only") {
    categories[[23]]$Title[i] <- paste(
      categories[[23]]$Title[i - 2], categories[[23]]$Title[i]
    )
  }
}

for (category in unique(wholeState$Subject)) {
  categories[[category]] <- categories[[category]] %>%
    pivot_wider(names_from = type, values_from = statistic)
}

combinedData <- do.call(rbind, categories)

combinedData <- combinedData %>%
  mutate(Low = Estimate - MOE) %>%
  mutate(High = Estimate + MOE)

combinedData <- combinedData[
  , c("Title", "state", "district", "Low", "Estimate", "High")
  ]

district1 <- combinedData %>% subset(district == 1)

# Models

# Predict Democrat or Republican 
# Predict Flipped Districts

# Statistics for data to be fit to model:

# - Population Density (Total population / Land Area)

# Sex and Age

# - Sex Ratio
# - Minors (Total population - 18 years and over) / (Total population)
# - Voting Age Population (18 years and over)
# - Seniors (65 years and over)
# - Median age (years)

# Race

# - Whites (White / Total population)
# - Blacks (Black or African American / Total population)
# - American Indian (American Indian and Alaska Native / Total population)
# - Pacific Islander (Native Hawaiian and Other Pacific Islander / Total population)
# - Other (Some other race / Total population)
# - Multiple Races (Two or more races / Total population)

# Hispanic or Latino and Race

# - Hispanic (Hispanic or Latino (of any race) / Total population)
# - Mexican (Mexican / Hispanic or Latino (of any race))
# - Puerto Rican (Puerto Rican / Hispanic or Latino (of any race))
# - Cuban (Cuban / Hispanic or Latino (of any race))
# - Other Hispanic (Other Hispanic or Latino / Hispanic or Latino (of any race))

# Place of Birth

# Natural born citizen (Native / Total Population)
# Born in State of residence (State of residence / Native)
# Born in Different state (Different state / Native)
# Born in US Territory, or born abroat to American parent(s)
#   (Born in Puerto Rico, U.S. Island areas, or born abroad to American parent(s) / Native)
# Naturalized Citizen (Foreign born / Total Population)

# Ancestry

# Disability Status of the Civilian Noninstitutionalized Population

# - (Voting Age) Disabled Population 
#     (Total civilian noninstitutionalized population With a disability - Under 18 years With a disability) /
#     (Total civilian noninstitutionalized population)

# Residence 1 Year Ago

# - Same house (Same house / Population 1 year and over)
# - Different county in same state (Different County / Population 1 year and over)
# - Different state (Different state / Population 1 year and over)
# - Abroad (Abroad / Population 1 year and over)

# Employment Status

# - Labor force participation (In labor force / Population 16 years and over)
# - Unemployment Rate
# - Servicemen (Armed Forces / Population 16 years and over)

# Commuting to Work

# - Car
#    (Car, truck, or van -- drove alone + Car, truck, or van -- carpooled) /
#    Workers 16 years and over
# - Walking or Public Transit 
#    (Public transportation (excluding taxicab) + Walked) /
#    Workers 16 years and over
# - Worked from home (Worked from home / Workers 16 years and over)
# - Mean travel time to work (minutes)

# Class of Worker 

# - Private sector (Private wage and salary worker / Civilian employed population 16 years and over)
# - Public sector (Government workers / Civilian employed population 16 years and over)
# - Self-employed (Self-employed in own not incorporated business workers / Civilian employed population 16 years and over)

