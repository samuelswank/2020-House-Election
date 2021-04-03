# 2020 House Election

## Project Overview

This project includes both a [shiny application](https://samuelswank.shinyapps.io/2020-House-Election/) and corresponding [R Markdown Notebook](https://rpubs.com/shengjiyang/House-2020). These grew out of a [Python project](https://github.com/shengjiyang/2016-House-Election) I completed examining the association of ethno-racial demographics and voter's choice of party in the national House of Representatives. Thanks to the Census Bureau’s [My Congressional District web app](https://www.census.gov/mycd/) making the relevant data readily available by House district in *.csv* files, I was able to significantly expand the number of different demographic categories used in this project.

In addition to exploring the influence of additional demographic factors on voter's choice of party, this notebook expands the inquiry to include the factors that caused certain districts to *flip party* this election.

## Directory Structure

```
├── .gitignore <- Files and directories to ignore in GitHub repo.
├── 2020-House-Flipped-Seats.Rproj <- R Studio project file
├── app.R <- shiny app file containing both ui and server
├── data
|    ├── census <- directory containing census data
|         ├── preprocessed <- directory containing preprocessed census data
|              ├── asianBreakdown.csv <- file containing proportions of different Asiatic nationalities in the counties comprising California's 39th and 48th district and Georgia's 7th district
|              ├── censusTidy.csv <- file containing modelData prior to feature engineering, standardization, and conversion to wide format
|              ├── hispanicBreakdown.csv <- file containing proportions of differenct Hispanic and Latino nationalities in counties comprising Florida's 26th and 27th Districts and New York's 11th and 22nd Districts
|              └── modelData.csv <- file containing preprocessed but unstandardized demographics used in partyModel.R file found in the helpers/model directory
|         └── raw <- directory containing raw census data in separate .csv files for each state from My Congressional District website
|    ├── houseResults.json <- .json file containing election results saved from Politico's map and balance of power page, NOTE: this file has been modified locally for ease in building the app and the margins of victory have not been updatated; therefore, its should not be used for research purposes
|    ├── images <- directory containing images and saved plots
|         ├── 11889226_120624232039.jpg <- Photograph of Representative Fletcher B. Swank
|         ├── FletcherBSwank.jpg <- Photograph of Representative Fletcher B. Swank
|         ├── plots <- directory containing modelTree plots
|              ├── modelTree1.png <- tree visualization from a single tree taken from Random Forest
|              └── modelTree2.png <- tree visualization from a single tree taken from Random Forest
|         ├── S001089.jpg <- Photograph of Representative Fletcher B. Swank
|         ├── SW001.jpg <- Photograph of Representative Fletcher B. Swank
|         └── WilliamClaySwankI.jpg <- Photograph of William Clay Swank I
|    ├── Oklahoma_64_to_72.geojson <- file containing historic Oklahoma Congressional District boundaries from the 64th through 72 congresses, used in easterEgg.R file found in the helpers/plotting directory
|    ├── Oklahoma_73_to_77.geojson <- file containing historic Oklahoma Congressional District boundaries from the 73rd through 77th congresses 
|    └── stfipsab.csv <- file containing state names, abbreviations and fips codes
├── dataWrangling
|    ├── censusPreprocessing.R <- data wrangling file for getting all of the census data into a single censusTidy.csv file and for performing feature engineering on said fail to create modelData.csv file
|    ├── houseResults.R <- data wrangling file for creating hr dataframe for use in creating modelData.csv file as well as the map.R file found in the helpers/plotting directory
├── helpers <- directory of helper functions used in app.R and notebook.Rmd
|    ├── model <- directory for modeling Rscripts used in app.R
|         ├── modelInfo.R <- Rscript for summarizing model performance metrics and plotting modelTree visualizations found in the data/images/plots directory
|         └── partyModel.R <- Rscript containing randomForest model
|    ├── plotting <- directory for plotting Rscripts
|         ├── easterEgg <- Rscript html elements and leaflet map found in the Oklahoma 5th District easter egg, used in app.R
|         ├── map.R <- Rscript plotting actual and predicted state and district maps
|         └── statistics.R <- Rscript plotting statistical visualizations
|    ├── stringManipulator.R <- helper file used in manipulating strings, used in various Rscripts throughout this repository
├── notebook <- directory of dataWrangling scripts solely in notebook.Rmd
|    ├── asianBreakdown.R <- Rscript used to create asianBreakdown.csv in data/census/preprocessed directory
|    ├── ballotpediaScraper.R <- Rscript used to process place of birth data
|    ├── densityAnalysis.R <- Rscript used to explore density distributions of flipped districts
|    └── hispanicBreakdown <- Rscript used to create hispanicBreakdown.csv in data/census/preprocessed directory
├── notebook.Rmd <- R Markdown Notebook
└── README.md  <- You are here! 2020-House-Election Repository README file
```

## Package and Versions

```
caTools      1.18.1
ggraph       2.0.5
ggrepel      0.9.1
glue         1.4.1.9000
gridExtra    2.3
gtools       3.8.2
igraph       1.2.6
leaflet      2.0.3
R.utils      2.10.1
randomForest 4.6.14
rgdal        1.5.18
rjson        0.2.20
rvest        0.3.6
scales       1.1.1
sf           0.9.6
shiny        1.5.0
sjmisc       2.8.5
tibble       3.0.4
tidyverse    1.3.0
tidycensus   0.10.2
tigris       1.0
usdata       0.1.0
yardstick    0.0.7
```