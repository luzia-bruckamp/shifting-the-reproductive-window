############### Reading in HFEA data ###############

# This file is part of the replication package for
# "Shifting the reproductive window: The contribution of ART and egg donation to fertility rates in the UK"
# by Luzia Bruckamp and Ester Lazzari

# The full replication package can be found here:
# https://github.com/luzia-bruckamp/shifting-the-reproductive-window

# Author: Luzia Bruckamp

# This file reads in the 7 HFEA data sets, cleans them,
# merges them, and exports them for analysis.
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
# First, deal with the data from 1991 to 2016, these 6 data sets
# are all in the same format

# function that selects and renames the specified variables
select_and_rename <- function(data) {
  data %>%
    select(
      age = Patient.Age.at.Treatment,
      previous_cycles = Total.Number.of.Previous.cycles..Both.IVF.and.DI,
      previous_cycles_ivf = Total.Number.of.Previous.IVF.cycles,
      previous_pregnancies = Total.number.of.previous.pregnancies..Both.IVF.and.DI,
      previous_live_births = Total.number.of.live.births...conceived.through.IVF.or.DI,
      type_treat = Type.of.treatment...IVF.or.DI,
      type_spec = Specific.treatment.type,
      egg_source = Egg.Source,
      sperm_source = Sperm.From,
      donated_embryo = Donated.embryo,
      reason = Main.Reason.for.Producing.Embroys.Storing.Eggs,
      egg_donor_age = Egg.Donor.Age.at.Registration,
      sperm_donor_age = Sperm.Donor.Age.at.Registration,
      stimulation = Stimulation.used,
      fresh = Fresh.Cycle,
      frozen = Frozen.Cycle,
      num_eggs = Fresh.Eggs.Collected,
      num_embryos_created = Total.Embryos.Created,
      year = Year.of.Treatment,
      year_b1 = Heart.One.Delivery.Date,
      year_b2 = Heart.Two.Delivery.Date,
      year_b3 = Heart.Three.Delivery.Date,
      year_b4 = Heart.Four.Delivery.Date,
      live_birth = Live.Birth.Occurrence,
      num_live_births = Number.of.Live.Births,
      num_embryos = Embryos.Transfered,
      num_embryos_ICSI = Embryos.Transfered.from.Eggs.Micro.injected,
      fetal_sacs = Number.of.foetal.sacs.with.fetal.pulsation
    )
}

# read in the data sets
data1 <- read.csv('hfea_1991_1994.csv')
data2 <- read.csv('hfea_1995_1999.csv')
data3 <- read.csv('hfea_2000_2004.csv')
data4 <- read.csv('hfea_2005_2009.csv')
data5 <- read.csv('hfea_2010_2014.csv')
data6 <- read.csv('hfea_2015_2016.csv')

# apply the function to all datasets
data_list <- list(data1, data2, data3, data4, data5, data6) 
data_list <- map(data_list, select_and_rename)

# get the cleaned data sets individually and put them in the global environment
# means we can easily access them individually
list2env(setNames(data_list, paste0("data", 1:6)), envir = .GlobalEnv)

# function to convert all columns to character
convert_to_character <- function(df) {
  df %>% mutate(across(everything(), as.character))
}

# convert all columns to character for all data sets
data_list <- map(data_list, convert_to_character)

# combine all data sets into one
combined_data <- bind_rows(data_list)

################### add 2017-2018 #################################
# data are in a slightly different format now, so the cleaning is
# a bit different and we have to make sure that everything works
# together

# this data set codes the larger numbers for previous (IVF and DI)
# cycles as >5 and for previous live births as >3
# replace by 99 to be able to use numeric values later

# this data set is also the only one that has the partner age which
# we will use for an analysis in the appendix

data7 <- read.csv('hfea_2017_2018.csv')

data7 <- data7 %>%
  select(age = Patient.age.at.treatment,
         previous_cycles_ivf = Total.number.of.previous.IVF.cycles,
         Total.number.of.previous.DI.cycles,
         previous_pregnancies = Total.number.of.previous.pregnancies...IVF.and.DI,
         previous_live_births = Total.number.of.previous.live.births...IVF.or.DI,
         type_treat = Type.of.treatment...IVF.or.DI,
         type_spec = Specific.treatment.type,
         egg_source = Egg.source,
         sperm_source = Sperm.source,
         donated_embryo = Donated.embryo,
         reason = Main.reason.for.producing.embroys.storing.eggs,
         egg_donor_age = Egg.donor.age.at.registration,
         sperm_donor_age = Sperm.donor.age.at.registration,
         stimulation = Stimulation.used,
         fresh = Fresh.cycle,
         frozen = Frozen.cycle,
         num_eggs = Fresh.eggs.collected,
         num_embryos_created = Total.embryos.created,
         year = Year.of.treatment,
         year_b1 = Heart.one.delivery.date,
         year_b2 = Heart.two.delivery.date,
         year_b3 = Heart.three.delivery.date,
         live_birth = Live.birth.occurrence,
         num_live_births = Number.of.live.births,
         num_embryos = Embryos.transferred,
         num_embryos_ICSI = Embryos.transferred.from.eggs.micro.injected,
         fetal_sacs = Number.of.foetal.sacs.with.fetal.pulsation,
         age_partner = Partner.age
  ) %>%
  mutate(
    previous_cycles_ivf = na_if(gsub(">5", "99", previous_cycles_ivf), ""),
    previous_cycles_ivf = as.numeric(previous_cycles_ivf),
    Total.number.of.previous.DI.cycles = na_if(gsub(">5", "99", Total.number.of.previous.DI.cycles), ""),
    Total.number.of.previous.DI.cycles = as.numeric(Total.number.of.previous.DI.cycles),
    previous_cycles = pmin(99, previous_cycles_ivf + Total.number.of.previous.DI.cycles),
    previous_live_births = na_if(gsub(">3", "99", previous_live_births), ""),
    year_b4 = NA_real_
  ) %>%
  select(age, previous_cycles, everything(), -Total.number.of.previous.DI.cycles
  ) %>%
  mutate(across(everything(), as.character))

# in order to merge the data sets, we need to add the partner age variable
# to the combined data before adding data7 (need the exact same variables
# to bind the rows)
combined_data$age_partner = "NA"

# put the datasets together
combined_data <- bind_rows(combined_data, data7)

################# count missing birth years ###################

# before cleaning, calculate what the share of missing birth years is
# for all successful treatments (we give this number in the paper)

n_successful_treatments <- combined_data %>%
  filter(type_treat == 'IVF',
         live_birth == 1) %>%
  pull() %>%
  length()

n_missing_birth_year <- combined_data %>%
  filter(type_treat == 'IVF',
         live_birth == 1,
         is.na(year_b1) | year_b1 == '999' | year_b1 == "") %>%
  pull() %>%
  length()

share_missing <- n_missing_birth_year/n_successful_treatments

# also look at the distribution of birth years (same year as treatment year or 
# 1 year after the treatment year)
# first do it by year
data_birth_distribution <- combined_data %>%
  mutate(year = as.numeric(year),
         year_b1 = as.numeric(year_b1)) %>%
  filter(type_treat == 'IVF',
         live_birth == 1) %>%
  mutate(same_year = ifelse(year_b1 == year, 1, 0),
         next_year = ifelse(year_b1 == year + 1, 1, 0)) %>%
  group_by(year) %>%
  summarise(all_live_births = n(),
            same_year_births = sum(same_year, na.rm = T),
            next_year_births = sum(next_year, na.rm = T)) %>%
  mutate(share_same_year = same_year_births/all_live_births,
         share_next_year = next_year_births/all_live_births,
         share_missing = 1-share_same_year-share_next_year,
         same_year_non_missing = share_same_year/(share_same_year+share_next_year))

# 1991 data only starts in August so all births occur in the next year
# 2016 data has only 2016 births and otherwise all are coded as missing
# In all other years, the split is very close to 30/70

# calculate split with all years, excluding 1991 and 2016
data_birth_distribution_all_years <- combined_data %>%
  mutate(year = as.numeric(year),
         year_b1 = as.numeric(year_b1)) %>%
  filter(type_treat == 'IVF',
         live_birth == 1,
         !(year %in% c(1991, 2016))) %>%
  mutate(same_year = ifelse(year_b1 == year, 1, 0),
         next_year = ifelse(year_b1 == year + 1, 1, 0)) %>%
  summarise(all_live_births = n(),
            same_year_births = sum(same_year, na.rm = T),
            next_year_births = sum(next_year, na.rm = T)) %>%
  mutate(share_same_year = same_year_births/all_live_births,
         share_next_year = next_year_births/all_live_births,
         share_missing = 1-share_same_year-share_next_year,
         same_year_non_missing = share_same_year/(share_same_year+share_next_year))

# overall split is 30.55% of non-missing births occur in the treatment year
# use 0.3 and 0.7 as probabilities when assigning missing years

##################### cleaning ####################################

# do all cleaning steps in dplyr pipeline

# set seed for reproducibility
set.seed(123)

cleaned_data <- combined_data %>%
  mutate(
    # standardise youngest age category (it sometimes has extra whitespace)
    age = gsub("18 - 34", "18-34", age),
    
    # convert variables with numbers 1 to 5 and ">=5" to numeric
    # replace ">=5" with 99
    # code any empty values with NAs
    # convert to numeric
    previous_cycles = na_if(gsub(">=5", "99", previous_cycles), ""),
    previous_cycles = as.numeric(previous_cycles),
    previous_cycles_ivf = na_if(gsub(">=5", "99", previous_cycles_ivf), ""),
    previous_cycles_ivf = as.numeric(previous_cycles_ivf),
    previous_pregnancies = na_if(gsub(">=5", "99", previous_pregnancies), ""),
    previous_pregnancies = as.numeric(previous_pregnancies),
    previous_live_births = na_if(gsub(">=5", "99", previous_live_births), ""),
    previous_live_births = as.numeric(previous_live_births),
    
    # Convert several vars to numeric
    year = as.numeric(year),
    num_live_births = as.numeric(num_live_births),
    donated_embryo = as.numeric(donated_embryo),
    stimulation = as.numeric(stimulation),
    fresh = as.numeric(fresh),
    frozen = as.numeric(frozen),
    
    # replace NAs in live births with 0 (no live birth is coded as NA)
    live_birth = replace(live_birth, is.na(live_birth), 0),
    
    # give egg donor age same format as patient age
    egg_donor_age = case_when(
      egg_donor_age == "<= 20" ~ "<=20",
      egg_donor_age %in% c("Between 21 and 25", "Between 26 and 30", "Between 31 and 35") ~ gsub("Between ", "", gsub(" and ", "-", egg_donor_age)),
      egg_donor_age == "999" ~ "999",
      egg_donor_age == "" ~ NA_character_,
      TRUE ~ egg_donor_age
    ),
    
    # also for sperm donor
    sperm_donor_age = case_when(
      sperm_donor_age == "<= 20" ~ "<=20",
      sperm_donor_age %in% c("Between 31 and 35", "Between 21 and 25", "Between 26 and 30", 
                             "Between 41 and 45", "Between 36 and 40") ~ gsub("Between ", "", gsub(" and ", "-", sperm_donor_age)),
      sperm_donor_age == "999" ~ "999",
      sperm_donor_age == "" ~ NA_character_,
      TRUE ~ sperm_donor_age
    ),
    
    # replace "not_assigned" with NA in sperm_source
    sperm_source = if_else(sperm_source == "not assigned", NA_character_, sperm_source),
    
    # create variables for treatment now, storage, donation, and research by
    # assigning all unique values of "reason" to the categories
    
    # variable that indicates treatment now
    treatment_now = if_else(
      reason %in% c(
        "Treatment Now ",
        "Treatment Now ,For Storing Embryos ",
        "Treatment Now ,For Donation ",
        "Treatment Now ,For Storing Eggs ",
        "Treatment Now ,For Donation ,For Storing Embryos ",
        "Treatment Now ,For Storing Embryos ,For Storing Eggs ",
        "Treatment Now ,For Research ",
        "Treatment Now ,For Donation ,For Storing Eggs ",
        "Treatment - IVF",
        "Treatment - DI",
        "Egg share"
      ),
      1, 0),
    
    # variable that indicates storage cycle
    primarily_storage = if_else(
      reason %in% c(
        "For Storing Embryos ",
        "For Donation ,For Storing Embryos ",
        "For Storing Eggs ",
        "For Storing Embryos ,For Storing Eggs ",
        "For Donation ,For Storing Eggs ",
        "For Storing Embryos ,For Storing Eggs ,For Research ",
        "Embryo storage",
        "Egg storage"
      ),
      1, 0),
    
    # variable that indicates donation cycle
    primarily_donation = if_else(
      reason %in% c(
        "For Donation ",
        "Donation"
      ), 
      1, 0),
    
    # variable that indicates research cycle
    primarily_research = if_else(
      reason == "For Research ",
      1, 0),
    
    # deal with number of eggs, format changed over time
    num_eggs_standardised = case_when(
      num_eggs %in% c("1", "2", "3", "4", "5") ~ "1-5",
      num_eggs %in% c("6", "7", "8", "9", "10") ~ "6-10",
      num_eggs %in% c("11", "12", "13", "14", "15") ~ "11-15",
      num_eggs %in% c("16", "17", "18", "19", "20") ~ "16-20",
      num_eggs %in% c("21", "22", "23", "24", "25") ~ "21-25",
      num_eggs %in% c("26", "27", "28", "29", "30") ~ "26-30",
      num_eggs %in% c("31", "32", "33", "34", "35") ~ "31-35",
      num_eggs %in% c("36", "37", "38", "39", "40") ~ "36-40",
      num_eggs == "0" ~ "0",
      num_eggs == "1-5" ~ "1-5",
      num_eggs == "6-10" ~ "6-10",
      num_eggs == "11-15" ~ "11-15",
      num_eggs == "16-20" ~ "16-20",
      num_eggs == "21-25" ~ "21-25",
      num_eggs == "26-30" ~ "26-30",
      num_eggs == "31-35" ~ "31-35",
      num_eggs == "36-40" ~ "36-40",
      as.numeric(num_eggs) > 40 | num_eggs == "> 50" | num_eggs == ">40" ~ ">40", # consolidate all over 40 into ">40"
      num_eggs == "" ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # same for embryos created
    num_embryos_created_standardised = case_when(
      num_embryos_created %in% c("1", "2", "3", "4", "5") ~ "1-5",
      num_embryos_created %in% c("6", "7", "8", "9", "10") ~ "6-10",
      num_embryos_created %in% c("11", "12", "13", "14", "15") ~ "11-15",
      num_embryos_created %in% c("16", "17", "18", "19", "20") ~ "16-20",
      num_embryos_created %in% c("21", "22", "23", "24", "25") ~ "21-25",
      num_embryos_created %in% c("26", "27", "28", "29", "30") ~ "26-30",
      num_embryos_created == "0" ~ "0",
      num_embryos_created == "1-5" ~ "1-5",
      num_embryos_created == "6-10" ~ "6-10",
      num_embryos_created == "11-15" ~ "11-15",
      num_embryos_created == "16-20" ~ "16-20",
      num_embryos_created == "21-25" ~ "21-25",
      num_embryos_created == "26-30" ~ "26-30",
      as.numeric(num_embryos_created) > 30 | num_embryos_created == "> 50" | num_embryos_created == ">30" ~ ">30", # consolidate all over 30 into ">30"
      num_embryos_created == "" | is.na(num_embryos_created) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # sum columns to create new variable total_embryos_transferred
    total_embryos_transferred = coalesce(as.numeric(num_embryos), 0) + coalesce(as.numeric(num_embryos_ICSI), 0),
    
    # create pregnancy indicator
    pregnancy = if_else(fetal_sacs > 0, 1, 0)
  ) %>%
  
  # delete variables that aren't needed
  select(-num_embryos, -num_embryos_ICSI, -fetal_sacs) %>%
  
  # replace NA with 0 in previous_live_births
  mutate(previous_live_births = replace_na(previous_live_births, 0),
         
         # generate a random birth year for each live birth so that we can use
         # it when the actual birth year is missing
         # with probability 0.3, the birth occurs in the same year as the treatment
         # with probability 0.7, the births occurs in the next year
         random_birth_year = case_when(
           live_birth == 1 ~ case_when(
             runif(n()) <= 0.3 ~ as.numeric(year),
             TRUE ~ as.numeric(year) + 1
           ),
           TRUE ~ NA_real_
         ),
         
         # now create year_birth using coalesce
         # there are up to 4 birth years recorded (up to 4 babies from one pregnancy)
         # in 2016, the birth year is coded as '999' in the majority of cases
         # therefore we always use the random birth year here
         # the data only starts in august 1991, all treatments in 1991 result in 
         # a birth in 1992
         # in all other years, we use the first non-NA birth year or the random birth year
         year_birth = case_when(
           # Special handling for 2016 - always use random assignment
           live_birth == 1 & year == 2016 ~ random_birth_year,
           # special handling for 1991 - always use 1992
           live_birth == 1 & year == 1991 ~ 1992,
           # normal handling for all other years
           live_birth == 1 ~ coalesce(
             as.numeric(year_b1),
             as.numeric(year_b2),
             as.numeric(year_b3),
             as.numeric(year_b4),
             random_birth_year
           ),
           TRUE ~ NA_real_
         ),
         
         # if year_birth is invalid (i.e. it is not the treatment year or the
         # year after the treatment), generate new random year
         year_birth = case_when(
           is.na(year_birth) ~ NA_real_,
           year_birth < year | year_birth > year + 1 ~ case_when(
             runif(n()) <= 0.3 ~ as.numeric(year),
             TRUE ~ as.numeric(year) + 1
           ),
           TRUE ~ year_birth
         )
  )

# delete the data that we don't need
cleaned_data <- cleaned_data %>%
  select(-year_b1, -year_b2, -year_b3, -year_b4, -random_birth_year)

# check for any conversion errors
summarise_all(cleaned_data, function(x) sum(is.na(x)))

################## export data ########################################

# export data set for use in subsequent analysis script

write.csv(cleaned_data, 'data_hfea_cleaned.csv')

