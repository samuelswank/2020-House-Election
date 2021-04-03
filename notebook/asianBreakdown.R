library(tidycensus)

asianVars <- c()

for (i in 2:9) {
  asianVars <- asianVars %>% append(paste("B02015_00", i, "E", sep = ""))
}

for (i in 10:25) {
  asianVars <- asianVars %>% append(paste("B02015_0", i, "E", sep = ""))
}

asianBreakdown <- get_acs(
  geography = "county",
  variables = asianVars,
  year = 2018,
  state = "California",
  summary_var = "B02015_001"
)

counties <- c(
  "Los Angeles County, California",
  "Orange County, California",
  "San Bernardino County, California"
)

asianBreakdown <- asianBreakdown %>%
  select(2, 3, 4, 6) %>%
  filter(NAME %in% counties)

avarNames <- c(
  "Asian Indian",
  "Bangladeshi",
  "Bhutanese",
  "Burmese",
  "Cambodian",
  "Chinese, except Taiwanese",
  "Filipino",
  "Hmong",
  "Indonesian",
  "Japanese",
  "Korean",
  "Laotian",
  "Malaysian",
  "Mongolian",
  "Nepalese",
  "Okinawan",
  "Pakistani",
  "Sri Lankan",
  "Taiwanese",
  "Thai",
  "Vietnamese",
  "Other Asian, specified",
  "Other Asian, not specified",
  "Two or more Asian"
)

asianBreakdown <- asianBreakdown %>%
  mutate(percentage = (estimate / summary_est) * 100) %>%
  select(1, 2, 5)

asianBreakdown <- asianBreakdown %>% spread(variable, percentage)
colnames(asianBreakdown)[2:ncol(asianBreakdown)] <- avarNames

asianBreakdown <- asianBreakdown %>%
  gather(
    key = "group",
    value = "percentage",
    `Asian Indian`:`Two or more Asian`,
    factor_key = TRUE
  )

colnames(asianBreakdown)[1] <- "County"