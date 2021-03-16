library(rvest)

birthplacePage <- read_html("https://ballotpedia.org/State_demographics_by_citizenship_status")

birthplaceData <- birthplacePage %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE) %>%
  .[[1]]

colnames(birthplaceData) <- c(
  "State",
  "Total Population",
  "Native",
  "Foreign-born Total",
  "Foreign-born Naturalized",
  "Non-citizen"
  )

birthplaceData <- birthplaceData[-c(1, 2, 105), ]
rownames(birthplaceData) <- NULL

birthplaceData <- birthplaceData[-seq(2, nrow(birthplaceData), 2), ]

for (column_ in colnames(birthplaceData)[2:ncol(birthplaceData)]) {
  birthplaceData[[column_]] <- lapply(
    birthplaceData[, column_], function(x) {as.numeric(gsub(",", "", x))}
    )
}

totalPop <- 0
for(i in 1:nrow(birthplaceData)) {
  totalPop <- totalPop + birthplaceData$`Total Population`[[i]][1]
}

totalNatural <- 0
for(i in 1:nrow(birthplaceData)) {
  totalNatural <- totalNatural + birthplaceData$`Native`[[i]][1]
}

totalNaturalized <- 0
for(i in 1:nrow(birthplaceData)) {
  totalNaturalized <- totalNaturalized + birthplaceData$`Foreign-born Naturalized`[[i]][1]
}


