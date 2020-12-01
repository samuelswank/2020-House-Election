library(tidyverse)

stateString <- function(selectedDistrict) {
  # Edge cases for multi-word state name
  # District of Columbia
  if (strsplit(selectedDistrict, " ")[[1]][1] == "District") {
    stateName <- "District of Columbia"
    
    # New
    # North / South
  } else if (
    strsplit(selectedDistrict, " ")[[1]][1] == "New" |
    strsplit(selectedDistrict, " ")[[1]][1] == "North" |
    strsplit(selectedDistrict, " ")[[1]][1] == "South"
  ) {
    stateName <- paste(
      strsplit(
        selectedDistrict, " ")[[1]][1], strsplit(selectedDistrict, " ")[[1]][2]
    )
  } else {stateName <- strsplit(selectedDistrict, " ")[[1]][1]}
  
  return (stateName)
}

districtString <- function(selectedDistrict, zero = FALSE) {
  # Edge cases for multi-word state name
  # District of Columbia
  if (strsplit(selectedDistrict, " ")[[1]][1] == "District") {
    districtNumber <- strsplit(selectedDistrict, " ")[[1]][5]
    
    # New
    # North / South
  } else if (
    strsplit(selectedDistrict, " ")[[1]][1] == "New" |
    strsplit(selectedDistrict, " ")[[1]][1] == "North" |
    strsplit(selectedDistrict, " ")[[1]][1] == "South"
  ) {districtNumber <- strsplit(selectedDistrict, " ")[[1]][4]}
  
  else {districtNumber <- strsplit(selectedDistrict, " ")[[1]][3]}
  
  if (zero == TRUE) {districtNumber <- paste("0", districtNumber, sep = "")}
  
  return(districtNumber)
}