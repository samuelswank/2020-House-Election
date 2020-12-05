library(tidyverse)

# al <- read_csv("data/Alabama_District_all.csv")
# 
# sexAge <- al %>% filter(Subject == "Sex and Age") %>% .[, 3:length(colnames(al))]
# 
# 
# for (col in sexAge %>% select(ends_with("Estimate")) %>% colnames()) {
#   sexAge[, col] <- sexAge[, col] %>% 
#     lapply(function(x) as.numeric(as.character(x)))
# } 
# 
# for (col in sexAge %>% select(ends_with("MOE")) %>% colnames()) {
#   sexAge[, col] <- sexAge[, col] %>% 
#     lapply(function(x) abs(as.numeric(gsub("[^0-9.-]", "", x))))
# }
# 
# sexAge <- sexAge %>% gather(district, statistic, 2:length(colnames(sexAge)))
# sexAge <- sexAge %>% separate(district, c("state", "district", "type"))
# 
# for (i in 1:length(sexAge$state)) {
#   sexAge[i, "state"] <- "Alabama"
# }
# 
# sexAge$district <- as.numeric(sexAge$district)
# 
# sexAge <- sexAge %>% spread(type, statistic)
# 
# sexAge <- sexAge %>% mutate(Low = Estimate - MOE)
# sexAge <- sexAge %>% mutate(High = Estimate + MOE)

al <- read_csv("data/Alabama_District_all.csv")[
  , 2:length(colnames(read_csv("data/Alabama_District_all.csv")))
  ]


for (col in al %>% select(ends_with("Estimate")) %>% colnames()) {
  al[, col] <- al[, col] %>% lapply(function(x) as.numeric(as.character(x)))
} 

for (col in al %>% select(ends_with("MOE")) %>% colnames()) {
  al[, col] <- al[, col] %>% 
    lapply(function(x) abs(as.numeric(gsub("[^0-9.-]", "", x))))
}

al <- al %>% gather(district, statistic, 3:length(colnames(al)))
al <- al %>% separate(district, c("state", "district", "type"))

for (i in 1:length(al$state)) {al[i, "state"] <- "Alabama"}

al$district <- as.numeric(al$district)

