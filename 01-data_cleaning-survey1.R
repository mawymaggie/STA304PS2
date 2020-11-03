#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from UCLA Nationscape Data Set.
# Author: Qiuyun Han, Wing Yi Ma, Tong Wu, Minhui Yu
# Data: 22 October 2020
# Contact: qiuyun.han@mail.utoronto.ca, yuminhui@mail.utoronto.ca, mawymaggie.ma@mail.utoronto.ca, tongtobey.wu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!
# GitHub Repo Link: https://github.com/mawymaggie/STA304PS3

#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/tongwu/Desktop/STA304/PS3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_survey <- read_dta("ns20200625.dta")
# Add the labels
raw_survey <- labelled::to_factor(raw_survey)
# Just keep some variables
reduced_survey <- 
  raw_survey %>% 
  select(vote_2020,
         vote_intention,
         registration,
         race_ethnicity,
         household_income,
         education,
         state,
         age)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?

reduced_survey<-
  reduced_survey %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

#filter for individuals who registered and decided to vote
filtered_survey<-reduced_survey %>%
  filter(vote_intention == "Yes, I will vote"& registration=="Registered"&
           (vote_2020=="Donald Trump"|vote_2020=="Joe Biden"))

#make age groups, map it to census data
filtered_survey<- filtered_survey %>%
  mutate(age_group= case_when(
    age <  20 ~ "Under 20",
    age >= 20 & age <= 39 ~ "20 to 40",
    age >= 40 & age <= 59 ~"40 to 60",
    age >= 60 & age <=79 ~  "60 to 80",
    age >= 80 ~ "Above 80"))


#combine educational data in survey and map it to census
filtered_survey<- filtered_survey%>%
  mutate(education= case_when(
    education == "3rd Grade or less" ~ "8th grade or less",
    education == "Middle School - Grades 4 - 8" ~ "8th grade or less",
    education == "Associate Degree" ~ "Associate Degree",
    education == "College Degree (such as B.A., B.S.)" ~ "College Degree (such as B.A., B.S.)",
    education == "High school graduate" ~"High school graduate",
    education == "Other post high school vocational training" ~ "High school graduate",
    education == "Completed some college, but no degree" ~ "Completed some college, but no degree",
    education == "Masters degree" ~ "Master degree",
    education == "Completed some graduate, but no degree" ~ "Completed some graduate, but no degree",
    education == "Completed some high school" ~ "Completed some high school",
    education == "Doctorate degree" ~ "Doctorate degree")
  )


filtered_survey<- filtered_survey %>%
  mutate(race= case_when(race_ethnicity== "White" ~ "White",
                         race_ethnicity== "Asian (Asian Indian)" ~ "Asian",
                         race_ethnicity== "Asian (Chinese)" ~ "Asian",
                         race_ethnicity== "Asian (Korean)" ~ "Asian",
                         race_ethnicity== "Asian (Japanese)" ~ "Asian",
                         race_ethnicity== "Asian (Other)" ~ "Asian",
                         race_ethnicity== "Asian (Filipino)" ~ "Asian",
                         race_ethnicity== "Asian (Vietnamese)" ~ "Asian",
                         race_ethnicity== "Black, or African American" ~ "Black",
                         race_ethnicity== "Pacific Islander (Samoan)" ~ "Pacific",
                         race_ethnicity== "Pacific Islander (Native Hawaiian)" ~ "Pacific",
                         race_ethnicity== "Pacific Islander (Other)" ~ "Pacific",
                         race_ethnicity== "Pacific Islander (Guamanian)" ~ "Pacific",
                         race_ethnicity== "American Indian or Alaska Native" ~ "American Native",
                         race_ethnicity== "Some other race" ~ "Other Races"))

#drop missing values
filtered_survey<-na.omit(filtered_survey)
filtered_survey <- filtered_survey%>% select(vote_2020, vote_trump,age,age_group,education,state,household_income,race)

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(filtered_survey, "survey_data.csv")

