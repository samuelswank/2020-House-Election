library(tidyverse)

al <- read_csv("data/Alabama_District_all.csv")

sexAge <- al %>% filter(Subject == "Sex and Age") %>% .[, 3:length(colnames(al))]


for (col in sexAge %>% select(ends_with("Estimate")) %>% colnames()) {
  sexAge[, col] <- sexAge[, col] %>% 
    lapply(function(x) as.numeric(as.character(x)))
} 

for (col in sexAge %>% select(ends_with("MOE")) %>% colnames()) {
  sexAge[, col] <- sexAge[, col] %>% 
    lapply(function(x) abs(as.numeric(gsub("[^0-9.-]", "", x))))
}

districts <- list()

for (i in 1:length(colnames(sexAge)) - 1) {
  districts[i] <- colnames(sexAge)[2:length(colnames(sexAge))][i] %>%
    strsplit(" ")
}

for (i in 1:length(districts)) {
  districts[i] <- paste(districts[[i]][1], districts[[i]][2])
}

for (i in 1:length(districts) / 2) {
  districts[i * 2] <- NULL
}

sexAge <- sexAge %>% gather(district, statistic, 2:length(colnames(sexAge)))
sexAge <- sexAge %>% separate(district, c("state", "district", "type"))

for (i in 1:length(sexAge$state)) {
  sexAge[i, "state"] <- "Alabama"
}

sexAge$district <- as.numeric(sexAge$district)

sexAge <- sexAge %>% spread(type, statistic)

sexAge <- sexAge %>% mutate(Low = Estimate - MOE)
sexAge <- sexAge %>% mutate(High = Estimate + MOE)

