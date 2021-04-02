library(tidycensus)

hispanicVars <- c()

for (i in 4:9) {
  hispanicVars <- hispanicVars %>% append(paste("B03001_00", i, "E", sep = ""))
}

for (i in 10:31) {
  hispanicVars <- hispanicVars %>% append(paste("B03001_0", i, "E", sep = ""))
}

hispanicBreakdown <- get_acs(
  geography = "county",
  variables = hispanicVars,
  year = 2018,
  state = c("Florida", "New York"),
  summary_var = "B03001_003"
  )

counties <- c(
  "Monroe County, Florida",
  "Miami-Dade County, Florida",
  "Chenango County, New York",
  "Cortland County, New York",
  "Madison County, New York",
  "Oneida County, New York",
  "Broome County, New York",
  "Herkimer County, New York",
  "Oswego County, New York",
  "Tioga County, New York",
  "Richmond County, New York",
  "Kings County, New York"
)

hispanicBreakdown <- hispanicBreakdown %>%
  select(2, 3, 4, 6) %>%
  filter(NAME %in% counties)

hvarNames <- c(
  "Mexican",
  "Puerto Rican",
  "Cuban",
  "Dominican",
  "Central American",
  "Costa Rican",
  "Guatemalan",
  "Honduran",
  "Nicaraguan",
  "Panamanian",
  "Salvadoran",
  "Other Central American",
  "South American",
  "Argentinean",
  "Bolivian",
  "Chilean",
  "Colombian",
  "Ecuadorian",
  "Paraguayan",
  "Peruvian",
  "Uruguayan",
  "Venezuelan",
  "Other South American",
  "Other Hispanic",
  "Spaniard",
  "Spanish",
  "Spanish American",
  "All Other Hispanic Groups"
)

hispanicBreakdown <- hispanicBreakdown %>%
  mutate(percentage = (estimate / summary_est) * 100) %>%
  select(1, 2, 5)

hispanicBreakdown <- hispanicBreakdown %>% spread(variable, percentage)
colnames(hispanicBreakdown)[2:ncol(hispanicBreakdown)] <- hvarNames

hispanicBreakdown <- hispanicBreakdown %>%
  gather(
    key = "group",
    value = "percentage",
    Mexican:`All Other Hispanic Groups`,
    factor_key = TRUE
    )

colnames(hispanicBreakdown)[1] <- "County"
