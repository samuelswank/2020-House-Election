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

- districtDemographics <- Congressional District, At Large indicates this is the only district alloted that state in the national House of Representatives
- sex_ratio <- Ratio of males to females normalized to 100
- minors <- Percentage of Minors to Total Population of Congressional District
- voting_age_pop <- Estimated Population 18 Years and Over
- seniors <- Percentage of Seniors to Estimated Voting Age Population
- med_age <- Median Age in Years
- white <- Percentage of Whites to Total Population of Congressional District
- black <- Percentage of Blacks to Total Population of Congressional District
- amerindian <- Percentage of American Indians to Total Population of Congressional District
- asian <- Percentage of Asians to Total Population of Congressional District
- other_race <- Percentage of Persons of Other Races to Total Population of Congressional District
- multiracial <- Percentage of Persons of Two or More Races to Total Population of Congressional district
- hispanic <- Percentage of Persons of Hispanic or Latino Origin of Any Race to the Total Population
- mexican <- Percentage of Persons of Mexican Origin to Persons of Hispanic or Latino Origin of Any Race
- puerto_rican <- Percentage of Persons of Puerto Rican Origin to Persons of Hispanic or Latino Origin of Any Race
- cuban <- Percentage of Persons of Cuban Origin to Persons of Hispanic or Latino Origin of Any Race
- other_hispanic <- Percentage of Persons of Other Hispanic or Latino Origins to Persons of Hispanic or Latino Origin of Any Race
- natural_born_citizen <- Percentage of Natural Born Citizens (those born on United States soil, or to American citizens abroad or in United States territories) to the Total Population of Citizens
- born_in_state <- Percentage of those Born in the Same State as Congressional District to Natural Born Citizens
- born_out_of_state <- Percentage of those Born in a Different State from Congressional District to Natural Born Citizens
- born_abroad <- Percentage of those Born in a Foreign Country or in a United States territory to Natural Born Citizens
- foreign_born <- Percentage of Naturalized citizens (immigrants) to the Total Population of Citizens
- disabled <- Percentage of Voting Age Non-Institutionalized Disabled Population to Total Non-institutionalized Population
- same_house <- Percentage of Population One Year of Age and Older which Lived in the Same House as the Previous Year to the Total Population One Year of Age and Older
- diff_county <- Percentage of Population One Year of Age and Older which Resided in a Different County in the Same State as the Previous Year to the Total Population One Year of Age and Older
- diff_state <- Percentage of Population One year of Age and Older which Resided in a Different State the Previous Year to the Total Population One Year of Age and Older
- abroad <- Percentage of Population One Year of Age and Older which Resided in a Different Country or in a United States Territory the Previous Year to the Total Population One Year of Age and older
- labor_force_participation <- Percentage of Population 16 Years of Age and Over In the Labor Force to the Total Population 16 Years of Age and Over
- unemployment <- Unemployment Rate as Measured by the United States Census
- servicemen <- Percentage of Men and Women Currently Serving in the Armed forces to the Total Population 16 Years of Age and Over
- car <- Percentage of Workers 16 Years of Age and Over who commuted by Car, Truck, or Van whether Alone or by Carpooling to the Total Population of Workers 16 Years of Age and Over
- walking_public_transit <- Percentage of Workers 16 Years of Age and Over who Commuted by way of Public Transportation, Excluding Taxicab, or who Walked to Work out of the Total Workers 16 Years of Age and Over
- commute <- Mean Travel Time to Work (minutes)
- private_sector <- Percentage of Private Wage and Salary Workers to the Total Civilian Population 16 Years and Over
- public_sector <- Percentage of Government Workers to the Total Civilian Population 16 Years and Over
- self_employed <- Percentage of Workers Employed in Companies they Own Non-incorporated Company to the Total Civilian Population 16 Years and Over
- homeowner_vacancy <- Homeowner Vacancy Rate
- rental_vacancy <- Rental Vacancy Rate
- owner_occupied <- Percentage of Owner-occupied Housing Units to Total Housing Units
- renter_occupied <- Percentage of Renter-occupied Housing Units to Total Housing Units
- two_or_less <- Percentage of Housing Units Occupied by Individuals who Moved in in 2017 or Later to the Total Number of Housing Units
- three_to_four <- Percentage of Housing Units Occupied by Individuals who Moved in in 2015 or 2016 to the Total Number of Housing Units
- five_to_nine <- Percentage of Housing Units Occupied by Individuals who moved in from 2010 or 2014 to the Total Number of Housing Units
- ten_to_nineteen <- Percentage of Housing Units Occupied by Individuals who Moved in from 2000 or 2009 to the Total Number of Housing Units
- twenty_to_twenty_nine <- Percentage of Housing Units Occupied by Individuals who moved in from 1990 or 1999 to the Total Number of Housing Units
- thirty_or_more <- Percentage of Housing Units Occupied by Individuals who Moved in from 1989 or Earlier to the Total Number of Housing Units
- med_smoc_mort <- Median Selected Monthly Owner Costs for Homeowners Currently Paying a Mortgage (dollars)
- med_smoc_no_mort <- Median Selected Monthly Owner Costs for Homeowners Not Currently Paying a Mortgage (dollars)
- med_rent <- Median Rent (dollars)
- med_hincome <- Median Household Income (dollars)
- mean_hincome <- Average Household Income (dollars)
- insured <- Percentage of Civilian Non-institutionalized Population With Health Insurance Coverage to Total Civilian Non-institutionalized Population
- private_coverage <- Percentage of Civilian Non-institutionalized Population With Private Health Insurance to Total Civilian Non-institutionalized Population
- public_coverage <- Civilian Non-institutionalized Population With Public Coverage to Total Civilian Non-institutionalized Psopulation
- uninsured <- Civilian Non-institutionalized Population Without Health Insurance Coverage to Total Civilian Non-institutionalized Population
- pov_families <- Poverty Rate among Families
- pov_mcouples <- Poverty Rate among Married Couples
- pov_single_mothers <- Poverty Rate among Single Mothers
- pov <- Poverty Rate among Total Population
- no_hsdiploma <- Percentage of Individuals with no High School Diploma, Equivalent, or Higher Degree to Total Population over 25
- some_college <- Percentage of individuals who have completed some college but never earned a degree to total population over 25
- associates <- Percentage of Individuals who have Earned an Associates Degree to Total Population over 25
- bachelors <- Percentage of Individuals who have Earned an Bachelors Degree to Total Population over 25
- graduate <- Percentage of Individuals who have Earned a Graduate Degree to Total Population over 25
- party <- Party Affiliation of district's representative in the House of Representative
- flipped <- Boolean value indicating whether the state had flipped party over the previous election