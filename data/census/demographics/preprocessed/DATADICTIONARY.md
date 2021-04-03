# Data Dictionary

## asianBreakdown.csv

- County <- Selected county from within California's 39th or 48th district, or Georgia's 7th district
- group  <- Nationality of Asiatic ethnic groups 
- percentage <- group's percentage of representation within County, according to the Census Bureau's 2018 American Community Survey (ACS)

# censusTidy.csv

- district <- Congressional district, At Large indicates this is the only district allot that state in the national House of Representatives
- Subject <- Subject as defined by the Census Bureau in the 2019 ACS to which the Title (demographic) belongs
- Title <- Selected demographic as outlined in the 2019 ACS under the stated Subject
- statistic <- Estimated number of individuals belonging to Title

## hispanicBreakdown.csv

- County <- Selected county from within Floridas's 26th or 27th district, or New York's 11th or 22nd district
- group  <- Nationality of Hispanic or Latino peoples 
- percentage <- group's percentage of representation within County, according to the 2018 ACS

## modelData.csv

- districtDemographics <- Congressional District, At Large indicates this is the only district allot that state in the national House of Representatives
- sex_ratio <- Ratio of males to females normalized to 100
- minors <- Percentage of minors to total population of congressional district
- voting_age_pop <- Estimated population 18 years and over
- seniors <- Percentage of seniors to estimated voting age population
- med_age <- Median age in years
- white <- Percentage of whites to total population of congressional district
- black <- Percentage of blacks to total population of congressional district
- amerindian <- Percentage of American Indians to total population of congressional district
- asian <- Percentage of Asians to total population of congressional district
- other_race <- Percentage of persons of other races to total population of congressional district
- multiracial <- Percentage of persons of two or more races to total population of congressional district
- hispanic <- Percentage of persons of Hispanic or Latino origin of any race to the total population
- mexican <- Percentage of persons of Mexican origin to persons of Hispanic or Latino origin of any race
- puerto_rican <- Percentage of persons of Puerto Rican origin to persons of Hispanic or Latino origin of any race
- cuban <- Percentage of persons of Cuban origin to persons of Hispanic or Latino origin of any race
- other_hispanic <- Percentage of persons of other Hispanic or Latino origins to persons of Hispanic or Latino origin of any race
- natural_born_citizen <- Percentage of Natural Born Citizens (those born on United States soil, or to American citizens abroad or in United States territories) to the total population of citizens
- born_in_state <- Percentage of those Born in the Same State as Congressional District to Natural Born Citizens
- born_out_of_state <- Percentage of those Born in a Different State from Congressional District to Natural Born Citizens
- born_abroad <- Percentage of those Born in a Foreign Country or in a United States territory to Natural Born Citizens
- foreign_born <- Percentage of Naturalized citizens (immigrants) to the total population of citizens
- disabled <- Percentage of Voting Age Non-Institutionalized Disabled Population to total non-institutionalized population
- same_house <- Percentage of population one year of age and older which lived in the same house as the previous year to the total population one year of age and older
- diff_county <- Percentage of population one year of age and older which resided in a different county in the same state as the previous year to the total population one year of age and older
- diff_state <- Percentage of population one year of age and older which resided in a different state the previous year to the total population one year of age and older
- abroad <- Percentage of population one year of age and older which resided in a different country or in a United States territory the previous year to the total population one year of age and older
- labor_force_participation <- Percentage of population 16 years of age and over In the Labor Force to the total population 16 years of age and over
- unemployment <- Unemployment rate as measured by the United States Census
- servicemen <- Percentage of Men and Women Currently Serving in the Armed forces to the total population 16 years of age and over
- car <- Percentage of Workers 16 Years of Age and Over who commuted by Car, Truck, or Van whether Alone or by Carpooling to the total population of workers 16 years of age and over
- walking_public_transit <- Percentage of Workers 16 Years of Age and Over who commuted by way of Public Transportation, Excluding Taxicab, or who Walked to Work out of the total workers 16 years of age and over
- commute <- Mean travel time to work (minutes)
- private_sector <- Percentage of Private Wage and Salary Workers to the total civilian population 16 years and over
- public_sector <- Percentage of Government Workers to the total civilian population 16 years and over
- self_employed <- Percentage of Workers Employed in Companies they Own non-Incorporated Company to the total civilian population 16 years and over
- homeowner_vacancy <- Homeowner Vacancy Rate
- rental_vacancy <- Rental Vacancy Rate
- owner_occupied <- Percentage of Owner-occupied Housing Units to total housing units
- renter_occupied <- Percentage of Renter-occupied Housing Units to total housing units
- two_or_less <- Percentage of Housing Units Occupied by Individuals who moved in in 2017 or Later to the total number of housing units
- three_to_four <- Percentage of Housing Units Occupied by Individuals who moved in in 2015 or 2016 to the total number of housing units
- five_to_nine <- Percentage of Housing Units Occupied by Individuals who moved in from 2010 or 2014 to the total number of housing units
- ten_to_nineteen <- Percentage of Housing Units Occupied by Individuals who moved in from 2000 or 2009 to the total number of housing units
- twenty_to_twenty_nine <- Percentage of Housing Units Occupied by Individuals who moved in from 1990 or 1999 to the total number of housing units
- thirty_or_more <- Percentage of Housing Units Occupied by Individuals who moved in from 1989 or Earlier to the total number of housing units
- med_smoc_mort <- Median Selected Monthly Owner Costs for Homeowners Currently Paying a Mortgage (dollars)
- med_smoc_no_mort <- Median Selected Monthly Owner Costs for Homeowners Not Currently Paying a Mortgage (dollars)
- med_rent <- Median Rent (dollars)
- med_hincome <- Median Household Income (dollars)
- mean_hincome <- Average Household Income (dollars)
- insured <- Percentage of Civilian non-institutionalized population With health insurance coverage to total civilian non-institutionalized population
- private_coverage <- Percentage of Civilian non-institutionalized population With private health insurance to total civilian non-institutionalized population
- public_coverage <- Civilian non-institutionalized population With public coverage to total civilian non-institutionalized population
- uninsured <- Civilian non-institutionalized population No health insurance coverage to total civilian non-institutionalized population
- pov_families <- Poverty rate among families
- pov_mcouples <- Poverty rate among married couples
- pov_single_mothers <- Poverty rate among single mothers
- pov <- Poverty rate among total population
- no_hsdiploma <- Percentage of individuals with no High School Diploma, Equivalent, or Higher Degree to total population over 25
- some_college <- Percentage of individuals who have completed some college but never earned a degree to total population over 25
- associates <- Percentage of individuals who have earned an Associates Degree to total population over 25
- bachelors <- Percentage of individuals who have earned an Bachelors Degree to total population over 25
- graduate <- Percentage of individuals who have earned a graduate degree to total population over 25
- party <- Party affiliation of district's representative in the House of Representative
- flipped <- Boolean value indicating whether the state had flipped party over the previous election