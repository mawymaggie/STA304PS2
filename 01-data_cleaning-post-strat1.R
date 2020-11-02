#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/tongwu/Desktop/STA304/PS3")
raw_census <- read_dta("usa_00002.dta")


# Add the labels
raw_census <- labelled::to_factor(raw_census)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_census <- 
  raw_census %>% 
  select(#region,
         stateicp,
         #sex, 
         age, 
         race, 
         #hispan,
         #marst, 
         #bpl,
         citizen,
         educd,
         hhincome)
         #labforce,
         #labforce)
         

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)


#filter for eligible voters
filtered_census<-reduced_census %>% filter(age >= 18)
filtered_census <- filtered_census %>% filter(citizen == "naturalized citizen" | citizen == "born abroad of american parents")

filtered_census <- 
  filtered_census %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")


#map race to survey data
filtered_census<- reduced_census %>% mutate(race= case_when(
  race == "white" ~ "White",
  race == "chinese" ~ "Asian",
  race == "japanese" ~ "Asian",
  race == "black/african american/negro" ~ "Black",
  race == "other asian or pacific islander" ~ "Pacific",
  race == "american indian or alaska native" ~ "American Native", 
  race == "other race, nec" ~ "Other Races",
  race == "two major races" ~ "Other Races",
  race == "three or more major races" ~ "Other Races"))

#make age groups to map with survey data
filtered_census$age <- as.integer(filtered_census$age)
filtered_census<-filtered_census %>%
  mutate(age_group= case_when(
    age <  20 ~ "Under 20",
    age >= 20 & age <= 39 ~ "20 to 40",
    age >= 40 & age <= 59 ~"40 to 60",
    age >= 60 & age <=79 ~  "60 to 80",
    age >= 80 ~ "Above 80"))

#filter education level
filtered_census<- filtered_census %>%
  mutate(education= case_when(educd == "nursery school to grade 4" ~ "8th grade or less",
                              educd == "nursery school, preschool" ~ "8th grade or less",
                              educd == "kindergarten" ~ "8th grade or less",
                              educd == "grade 1, 2, 3, or 4" ~ "8th grade or less",
                              educd == "grade 1" ~ "8th grade or less",
                              educd == "grade 2" ~ "8th grade or less",
                              educd == "grade 3" ~ "8th grade or less",
                              educd == "grade 4" ~ "8th grade or less",
                              educd == "grade 5, 6, 7, or 8" ~ "8th grade or less",
                              educd == "grade 5 or 6" ~ "8th grade or less",
                              educd == "grade 5" ~ "8th grade or less",
                              educd == "grade 6" ~ "8th grade or less",
                              educd == "grade 7" ~ "8th grade or less",
                              educd == "grade 8" ~ "8th grade or less",
                              educd == "grade 7 or 8" ~ "8th grade or less",
                              educd == "grade 9" ~ "Completed some high school",
                              educd == "grade 10" ~ "Completed some high school",
                              educd == "grade 11" ~ "Completed some high school",
                              educd == "grade 12" ~ "Completed some high school",
                              educd == "12th grade, no diploma" ~ "Completed some high school",
                              educd == "high school graduate or GED" ~ "High school graduate",
                              educd == "regular high school diploma" ~ "High school graduate",
                              educd == "ged or alternative credential" ~ "High school graduate",
                              educd == "some college, but less than 1 year" ~ "Completed some college, but no degree",
                              educd == "1 or more years of college credit, no degree" ~ "Completed some college, but no degree",
                              educd == "1 year of college" ~ "Completed some college, but no degree",
                              educd == "2 years of college" ~ "Completed some college, but no degree",
                              educd == "3 years of college" ~ "Completed some college, but no degree",
                              educd == "4 years of college" ~ "Completed some college, but no degree",
                              educd == "5+ years of college" ~ "Completed some college, but no degree",
                              educd == "6 years of college (6+ in 1960-1970)" ~ "Completed some college, but no degree",
                              educd == "7 years of college" ~ "Completed some college, but no degree",
                              educd == "8+ years of college" ~ "Completed some college, but no degree",
                              educd == "bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                              educd == "associate's degree, type not specified" ~ "Associate Degree",
                              educd == "associate's degree, occupational program" ~ "Associate Degree",
                              educd == "associate's degree, academic program" ~ "Associate Degree",
                              educd == "master's degree" ~ "Master degree",
                              educd == "doctoral degree" ~ "Doctorate degree"))


#map household income to survey data
#filter extreme values in household income
filtered_census$hhincome<-ifelse(filtered_census$hhincome==9999999, NaN,filtered_census$hhincome)
filtered_census<-filtered_census %>%
  mutate(household_income = case_when(hhincome<=14999 ~ "Less than $14,999", 
                                      hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999", 
                                      hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999", 
                                      hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999", 
                                      hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999", 
                                      hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999", 
                                      hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999", 
                                      hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999", 
                                      hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999", 
                                      hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                                      hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999", 
                                      hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999", 
                                      hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999", 
                                      hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999", 
                                      hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999", 
                                      hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999", 
                                      hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999", 
                                      hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999", 
                                      hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999", 
                                      hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999", 
                                      hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999", 
                                      hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999", 
                                      hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999", 
                                      hhincome>=250000~"$250,000 and above")) 
filtered_census$hhincome<-NULL



#map the state to survey data
filtered_census<-filtered_census %>% 
  mutate(state = case_when(stateicp=="alabama"~"AL", 
                           stateicp=="alaska"~"AK",
                           stateicp=="arizona"~"AZ", 
                           stateicp=="arkansas"~"AR", 
                           stateicp=="california"~"CA", 
                           stateicp=="colorado"~"CO", 
                           stateicp=="connec1cut"~"CT", 
                           stateicp=="delaware"~"DE", 
                           stateicp=="florida"~"FL",
                           stateicp=="georgia"~"GA", 
                           stateicp=="hawaii"~"HI", 
                           stateicp=="idaho"~"ID", 
                           stateicp=="illinois"~"IL",
                           stateicp=="indiana"~"IN", 
                           stateicp=="iowa"~"IA", 
                           stateicp=="kansas"~"KS", 
                           stateicp=="kentucky"~"KY", 
                           stateicp=="louisiana"~"LA", 
                           stateicp=="maine"~"ME", 
                           stateicp=="maryland"~"MD", 
                           stateicp=="massachusews"~"MA", 
                           stateicp=="michigan"~"MI", 
                           stateicp=="minnesota"~"MN", 
                           stateicp=="mississippi"~"MS", 
                           stateicp=="missouri"~"MO",
                           stateicp=="montana"~"MT", 
                           stateicp=="nebraska"~"NE", 
                           stateicp=="nevada"~"NV",
                           stateicp=="new hampshire"~"NH", 
                           stateicp=="new jersey"~"NJ", 
                           stateicp=="new mexico"~"NM", 
                           stateicp=="new york"~"NY", 
                           stateicp=="north carolina"~"NC", 
                           stateicp=="north dakota"~"ND",
                           stateicp=="ohio"~"OH", 
                           stateicp=="oklahoma"~"OK", 
                           stateicp=="oregon"~"OR", 
                           stateicp=="pennsylvania"~"PA", 
                           stateicp=="rhode island"~"RI", 
                           stateicp=="south carolina"~"SC", 
                           stateicp=="south dakota"~"SD", 
                           stateicp=="tennessee"~"TN",
                           stateicp=="texas"~"TX", 
                           stateicp=="utah"~"UT", 
                           stateicp=="vermont"~"VT",
                           stateicp=="virginia"~"VA",
                           stateicp=="washington"~"WA", 
                           stateicp=="west virginia"~"WV", 
                           stateicp=="wisconsin"~"WI", 
                           stateicp=="wyoming"~"WY", 
                           stateicp=="district of columbia"~"DC"))
filtered_census$stateicp<-NULL



#filter missing values
filtered_census<-na.omit(filtered_census)

filtered_census <- 
  filtered_census %>%
  count(age_group,race,household_income, education, state) %>%
  group_by(age_group,race,household_income,education,state)


  
# Saving the census data as a csv file in my
# working directory
write_csv(filtered_census, "census_data.csv")



         