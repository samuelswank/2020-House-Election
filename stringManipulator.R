library(tidyverse)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

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