############### Reading in HFD data ###############

# This file is part of the replication package for
# "Shifting the reproductive window: The contribution of ART and egg donation 
# to fertility rates in the UK"
# by Luzia Bruckamp and Ester Lazzari

# The full replication package can be found here:
# https://github.com/luzia-bruckamp/shifting-the-reproductive-window

# Author: Luzia Bruckamp

# This file reads in the three relevant .txt files from the HFD,
# cleans them, and converts them to .csv.
# Run this file before "analysis.R"!

rm(list = ls())

# Set working directory to where the data files are
# and where intermediate files will be exported
# YOU HAVE TO CHANGE THIS PATH TO MATCH YOUR FILE LOCATION
wd <- 'your/path/here' # CHANGE THIS

setwd(wd)

# if the relevant package below is not installed yet, you will have to install
# it with the following command:
# install.packages('tidyverse')

# load relevant packages
# R version used was 4.2.2
library(tidyverse) # version 2.0.0

######################## read in data ###############################

# for each data set, skip the first two rows because they contain a general
# description

# total fertility rate
data_TFR <- read.table("GBR_NPtfrRR.txt", 
                       header = TRUE, 
                       sep = "", 
                       skip = 2)

# live births by year and age
data_births <- read.table("GBR_NPbirthsRR.txt", 
                   header = TRUE, 
                   sep = "", 
                   skip = 2)

# female population by year and age
data_exp <- read.table("GBR_NPexposRR.txt", 
                       header = TRUE, 
                       sep = "", 
                       skip = 2)

######################## clean data ###############################

# selecting the relevant variables, relevant time frame, and generating
# the same age groups as in the HFEA data

# total fertility rate
data_TFR <- data_TFR %>%
  select(year = Year,
         tfr = TFR) %>%
  filter(year >= 1991 & year <= 2019)

# live births by year and age
data_births <- data_births %>%
  select(year = Year,
         age = Age,
         Total) %>%
  filter(year >= 1991 & year <= 2019,
         age >= 18 & age <= 50) %>% 
  mutate(age_group = case_when(
    age >= 18 & age <= 34 ~ "18-34",
    age >= 35 & age <= 37 ~ "35-37",
    age >= 38 & age <= 39 ~ "38-39",
    age >= 40 & age <= 42 ~ "40-42",
    age >= 43 & age <= 44 ~ "43-44",
    age >= 45 & age <= 50 ~ "45-50"
  )) %>%
  group_by(year, age_group) %>%
  summarise(total_births = sum(Total)) %>%
  rename(age = age_group)

# female population by year and age
# also add the number of single years of age in each age group
# this is needed for TFR calculations later
data_exp <- data_exp %>%
  rename(year = Year,
         age = Age,
         population = Exposure) %>%
  filter(year >= 1991 & year <= 2019,
         age >= 18 & age <= 50) %>% 
  mutate(age_group = case_when(
    age >= 18 & age <= 34 ~ "18-34",
    age >= 35 & age <= 37 ~ "35-37",
    age >= 38 & age <= 39 ~ "38-39",
    age >= 40 & age <= 42 ~ "40-42",
    age >= 43 & age <= 44 ~ "43-44",
    age >= 45 & age <= 50 ~ "45-50"
  )) %>%
  group_by(year, age_group) %>%
  summarise(population = sum(population)) %>%
  rename(age = age_group) %>%
  # add the number of years in each age group, important for TFR later
  mutate(num_years = case_when(
    age >= 18 & age <= 34 ~ 17,
    age >= 35 & age <= 37 ~ 3,
    age >= 38 & age <= 39 ~ 2,
    age >= 40 & age <= 42 ~ 3,
    age >= 43 & age <= 44 ~ 2,
    age >= 45 & age <= 50 ~ 6
  ))

######################## export data ###############################

# save cleaned data
write.csv(data_TFR, "tfr_by_year.csv", row.names = FALSE)
write.csv(data_births, "births_by_age_year.csv", row.names = FALSE)
write.csv(data_exp, "female_pop_by_age_year.csv", row.names = FALSE)
