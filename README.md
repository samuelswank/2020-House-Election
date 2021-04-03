# 2020 House Election

## Project Overview

| This project includes both a [shiny application](https://samuelswank.shinyapps.io/2020-House-Election/) and corresponding [R Markdown Notebook](https://rpubs.com/shengjiyang/House-2020). These grew out of a [Python project](https://github.com/shengjiyang/2016-House-Election) I completed examining the association of ethno-racial demographics and voter's choice of party in the national House of Representatives. Thanks to the Census Bureau’s [My Congressional District web app](https://www.census.gov/mycd/) making the relevant data readily available by House district in *.csv* files, I was able to significantly expand the number of different demographic categories used in this project.

\  

| In addition to exploring the influence of additional demographic factors on voter's choice of party, this notebook expands the inquiry to include the factors that caused certain districts to *flip party* this election.

## Directory Structure

```
├── .gitignore <- Files and directories to ignore in GitHub repo.
├── 2020-House-Flipped-Seats.Rproj <- R Studio project file
├── app.R <- shiny app file containing both ui and server
├── data
|    ├── census <- directory containing census data
|         ├──
|    ├── houseResults.json <- .json file containing election results saved from Politico's map and balance of power page, NOTE: this file has been modified locally for ease in building the app and the margins of victory have not been updatated; therefore, its should not be used for research purposes
|    ├── images <- directory containing images and saved plots
|         ├──
|    ├── Oklahoma_64_to_72.geojson <- file containing historic Oklahoma Congressional District boundaries from the 64th through 72 congresses, used in easterEgg.R file found in the helpers/plotting directory
|    ├── Oklahoma_73_to_77.geojson <- file containing historic Oklahoma Congressional District boundaries from the 73rd through 77th congresses 
|    ├── predictions <- directory containing images and saved plots
|         ├──
|    ├── index2015_data.xls <- Heritage Foundation's Index of Economic Freedom Data for Year 2015
|    ├── index2016_data.xls <- Heritage Foundation's Index of Economic Freedom Data for Year 2016
|    ├── index2017_data.xls <- Heritage Foundation's Index of Economic Freedom Data for Year 2017
|    ├── index2018_data.xls <- Heritage Foundation's Index of Economic Freedom Data for Year 2018
|    ├── index2019_data.xls <- Heritage Foundation's Index of Economic Freedom Data for Year 2019
|    ├── index2020_data.xls <- Heritage Foundation's Index of Economic Freedom Data for Year 2020
|    └── sample.csv <- file containing sample countries and territory data used in creating adjusted.csv
├── plots
|    ├── TariffGDPGrowth.png <- ggplot2 visualization of GDP Growth Rate's relationship to the previous year's tariff rate for sample countries in 2017
|    └── TariffGDPperCapita.png <- ggplot2 visualization of Per Capita GDP's relationship to the previous year's tariff rate for sample countries from 2013-2017
└── README.md  <- You are here! 2020-House-Election Repository README file
```