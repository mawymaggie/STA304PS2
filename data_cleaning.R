library(haven)
library(tidyverse)
#### *****Data Selection & Cleaning*****####
#### Survey data -Load and select####
raw_survey <- read_dta("ns20200625/ns20200625.dta")
raw_survey <- labelled::to_factor(raw_survey)
raw_survey
# Just keep some variables-make sure it exists in census data as well (except for vote intention)
reduced_survey <-
  raw_survey %>%
  select(vote_2020,
         vote_intention,
         registration,
         age,
         education,
         state,
         household_income,
         race_ethnicity)
#Adjust Data types
reduced_survey$age<-as.numeric(reduced_survey$age)
# Filter on survey data
#filter only on the people that are both registered & intented to vote (Optional, depends on your assumptions)
#(Assuming people will vote unless they explicitly say no)
filtered_survey<-reduced_survey %>%
  filter(registration=="Registered"&
           vote_intention == "Yes, I will vote"&
           (vote_2020=="Donald Trump"|vote_2020=="Joe Biden")
  )
#Drop NAs (4296 out of 6479, 66% data kept)
filtered_survey<-na.omit(filtered_survey)
rm(raw_survey,reduced_survey)
####Census data- Load and select####
raw_census <- read_dta("usa_00001.dta.gz")
raw_census <- labelled::to_factor(raw_census)
# Just keep some variables
reduced_census <-
  raw_census %>%
  select(perwt,
         citizen,
         age,
         educd,
         stateicp,
         hhincome,
         race
  )
#Change data types
reduced_census$age<-as.numeric(reduced_census$age)
#reduced_data_census$inctot<-as.numeric(reduced_data_census$inctot)
#Filter Census data- only keeping those who can vote
filtered_census<-reduced_census %>% filter(age>=18 & (citizen=="naturalized citizen"|
                                                        citizen=="born abroad of american parents"))
#Adjust some NAs
filtered_census$hhincome<-ifelse(filtered_census$hhincome==9999999,
                                 NaN,filtered_census$hhincome)
#Drop NAs (222298/228159, 97% data kept)
filtered_census<-na.omit(filtered_census)
rm(raw_census,reduced_census)
####Map data style between survey & census####
#####Create Age group in both datasets####
#AGE in survey
filtered_survey<- filtered_survey %>%
  mutate(agegroup= case_when(age < 20 ~ "Under 20",
                             age >= 20 & age <= 39 ~ "20 to 40",
                             age >= 40 & age <= 59 ~"40 to 60",
                             age >= 60 & age <=79 ~ "60 to 80",
                             age >= 80 ~ "Above 80"))
#AGE in census
filtered_census<-filtered_census %>%
  mutate(agegroup = case_when(age < 20 ~ "Under 20",
                              age >= 20 & age <= 39 ~ "20 to 40",
                              age >= 40 & age <= 59 ~"40 to 60", 
                              age >= 60 & age <=79 ~ "60 to 80",
                              age >= 80 ~ "Above 80"))
unique(filtered_census$agegroup)
unique(filtered_survey$agegroup)
####Map education/educd column####
#EDUCATION in Survey
filtered_survey<- filtered_survey%>%
  mutate(education = case_when(
    education == "3rd Grade or less" ~ "8th grade or less",
    education == "Middle School - Grades 4 - 8" ~ "8th grade or less",
    education == "Completed some high school" ~ "Completed some high school",
    education == "High school graduate" ~ "High school graduate",
    education == "Other post high school vocational training" ~ "Other post high school vocational training",
    education == "Completed some college, but no degree" ~ "Completed some college, but no degree",
    education == "Associate Degree" ~ "Associate Degree",
    education == "College Degree (such as B.A., B.S.)" ~ "College Degree (such as B.A., B.S.)",
    education == "Completed some graduate, but no degree" ~ "Completed some graduate, but no degree",
    education == "Masters degree" ~ "Masters degree",
    education == "Doctorate degree" ~ "Doctorate degree")
  )

#EDUCATION in Census
filtered_census<- filtered_census %>%
  mutate(educd = case_when(
    educd == "no schooling completed" ~ "8th grade or less",
    educd == "nursery school to grade 4" ~ "8th grade or less",
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
    educd == "master's degree" ~ "Masters degree",
    educd == "professional degree beyond a bachelor's degree" ~ "Professional degree beyond a bachelor's degree",
    educd == "doctoral degree" ~ "Doctorate degree")
  )
unique(filtered_census$educd)
unique(filtered_survey$education)
####Map Sate/Stateicp####
filtered_census<-filtered_census %>%
  mutate(state = case_when(stateicp=="alabama"~"AL",
                           stateicp=="alaska"~"AK",
                           stateicp=="arizona"~"AZ",
                           stateicp=="arkansas"~"AR",
                           stateicp=="california"~"CA",
                           stateicp=="colorado"~"CO",
                           stateicp=="connecticut"~"CT",
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
unique(filtered_census$state)
unique(filtered_survey$state)

####Map household income####
x<-unique(filtered_survey$household_income)
min(filtered_census$hhincome)
max(filtered_census$hhincome)
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
                                      hhincome>=250000~"$250,000 and above"
  ))
filtered_census$hhincome<-NULL
unique(filtered_census$household_income)
unique(filtered_survey$household_income)
#Map Variables RACE
#RACE in survey
filtered_survey<- filtered_survey %>%
  mutate(race= case_when(
    race_ethnicity == "White" ~ "White",
    race_ethnicity == "Asian (Asian Indian)" ~ "Asian",
    race_ethnicity == "Asian (Chinese)" ~ "Asian",
    race_ethnicity == "Asian (Korean)" ~ "Asian",
    race_ethnicity == "Asian (Japanese)" ~ "Asian",
    race_ethnicity == "Asian (Other)" ~ "Asian",
    race_ethnicity == "Asian (Filipino)" ~ "Asian",
    race_ethnicity == "Asian (Vietnamese)" ~ "Asian",
    race_ethnicity == "Black, or African American" ~ "Black",
    race_ethnicity == "Pacific Islander (Samoan)" ~ "Pacific",
    race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "Pacific",
    race_ethnicity == "Pacific Islander (Other)" ~ "Pacific",
    race_ethnicity == "Pacific Islander (Guamanian)" ~ "Pacific",
    race_ethnicity == "American Indian or Alaska Native" ~ "American Native",
    race_ethnicity == "Some other race" ~ "Other Races"
  ))
#RACE in Census 
filtered_census<- filtered_census %>%
  mutate(race= case_when(
    race == "white" ~ "White",
    race == "chinese" ~ "Asian",
    race == "japanese" ~ "Asian",
    race == "black/african american/negro" ~ "Black",
    race == "other asian or pacific islander" ~ "Pacific",
    race == "american indian or alaska native" ~ "American Native",
    race == "other race, nec" ~ "Other Races",
    race == "two major races" ~ "Other Races",
    race == "three or more major races" ~ "Other Races"))
unique(filtered_census$race)
unique(filtered_survey$race)

