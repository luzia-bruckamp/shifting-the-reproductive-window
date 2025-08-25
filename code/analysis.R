############### Full analysis of HFEA data ###############

# This file is part of the replication package for
# "Shifting the reproductive window: The contribution of ART and egg donation to fertility rates in the UK"
# by Luzia Bruckamp and Ester Lazzari

# The full replication package can be found here:
# https://github.com/luzia-bruckamp/shifting-the-reproductive-window

# Author: Luzia Bruckamp

# This file reads in the cleaned data from the previous scripts
# and replicates all analyses and figures from the paper.
# Make sure you run "cleaning_hfd.R" and "cleaning_hfea.R" before
# running this script!

rm(list = ls())

# Set working directory to where the data files are
# and where intermediate files will be exported
# YOU HAVE TO CHANGE THIS PATH TO MATCH YOUR FILE LOCATION
wd <- 'your/path/here' # CHANGE THIS

setwd(wd)

# if the relevant packages below aren't installed yet, you will have to install
# them with the following commands:
# install.packages('tidyverse')
# install.packages('patchwork')

# load relevant packages
# R version used was 4.2.2
library(tidyverse) # version 2.0.0
library(patchwork) # version 1.3.0

####################### HFEA data ###################################

# load the data, do some filtering

# from data cleaning script
data_unfiltered <- read.csv('data_hfea_cleaned.csv')

# the data contains ART and donor insemination cycles, we only want the
# ART cycles
data_raw <- data_unfiltered %>%
  filter(type_treat == 'IVF')

# immediately do some filtering steps that are needed almost everywhere
# we only want "treatment now" cycles and we need the age group assigned
data <- data_raw %>%
  filter(primarily_storage == 0,
         primarily_donation == 0,
         primarily_research == 0,
         age != '999')

# filter for no previous ART cycles, only new patients who are going 
# for their first cycle
data_first <- data %>%
  filter(previous_cycles_ivf == 0)

# second and third tries
data_second <- data %>%
  filter(previous_cycles_ivf == 1)

data_third <- data %>%
  filter(previous_cycles_ivf == 2)

############# count number of treatments and patients etc. #################

# numbers that we need for the description of the data set

# ART cycles in the raw data
n_all_art_data <- data_raw %>%
  pull() %>%
  length()

# number of cycles we have to exclude because of missing age 
n_exclude <- data_raw %>%
  filter(primarily_storage == 0,
         primarily_donation == 0,
         primarily_research == 0,
         age == '999') %>%
  pull() %>%
  length()

# check what proportion of the data are storage cycles
n_storage <- data_raw %>%
  filter(primarily_storage == 1) %>%
  pull() %>%
  length()

share_storage <- n_storage/n_all_art_data

# check what proportion of the data are donation cycles
n_donation <- data_raw %>%
  filter(primarily_donation == 1) %>%
  pull() %>%
  length()

share_donation <- n_donation/n_all_art_data

# check what proportion of the data are research cycles
n_research <- data_raw %>%
  filter(primarily_research == 1) %>%
  pull() %>%
  length()

share_research <- n_research/n_all_art_data

# check what proportion of the data are treatment cycles
n_treatment <- data_raw %>%
  filter(primarily_research == 0,
         primarily_donation == 0,
         primarily_storage == 0) %>%
  pull() %>%
  length()

share_treatment <- n_treatment/n_all_art_data

# proportion of fresh transfers for the methodology section
prop_fresh_frozen <- data_first %>%
  group_by(age, egg_source) %>%
  summarise(num_fresh = sum(fresh == 1),
            num_frozen = sum(fresh == 0)) %>%
  mutate(total = num_fresh + num_frozen,
         prop_fresh = num_fresh/total)

####################### HFD data ###################################

# total fertility rate
data_TFR <- read.csv('tfr_by_year.csv')

# all births by age and year
data_births <- read.csv('births_by_age_year.csv')

# female population by age and year
data_exp <- read.csv('female_pop_by_age_year.csv')

#################### set colours for all age group plots ####################

colours_age <- c("#729EA1", "#B5BD89", "#DFBE99", "#EC9192", "#DB5375", "#AB0707")

############## Table 1: count observations #################

# count patients and treatments in each year
# count donor egg treatments and calculate share of donor egg treatments
data_tab1 <- data %>%
  group_by(year) %>%
  summarise(patients = sum(previous_cycles_ivf == 0),
            treatments = n(),
            donor_treatments = sum(egg_source == "Donor"),
            share_donor = round((donor_treatments/treatments*100), digits = 1)) %>%
  mutate(year = as.character(year))

# calculate the total over the whole time period
total_row <- data_tab1 %>%
  summarise(
    year = "Total",
    patients = sum(patients, na.rm = TRUE),
    treatments = sum(treatments, na.rm = TRUE),
    donor_treatments = sum(donor_treatments, na.rm = TRUE)
  ) %>%
  mutate(
    share_donor = round((donor_treatments / treatments) * 100, digits = 1)
  )

# add total row to yearly data
data_tab1 <- bind_rows(data_tab1, total_row)

############## Figure 1: treatment rates #################

# first-time treatments as the numerator and total number of women
# as the denominator (separately for age groups and egg source)

# count first time treatments by year, age group, and egg source
data_treat <- data_first %>%
  group_by(year, age, egg_source) %>%
  summarise(count = n())

# add female population data by year and age group, calculate treatment rate
data_fig1 <- left_join(data_treat, data_exp) %>%
  mutate(treat_rate = count/population * 1000,
         egg_source = factor(egg_source, levels = c("Patient", "Donor"),
                             labels = c("(a) Autologous eggs", "(b) Donor eggs")))

# plot
ggplot(data_fig1, aes(x = year, y = treat_rate, group = age, colour = age)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "",
    x = "Year", y = "First treatments per 1,000 women",
    colour = "Age"
  ) +
  theme_minimal() +
  facet_wrap(~egg_source, scales = "free_y") +
  scale_color_manual(values = colours_age) +
  theme(text = element_text(family = "Times New Roman"))

# bring the data in the format of the table for the appendix
data_fig1_autologous <- data_fig1 %>%
  filter(egg_source == "(a) Autologous eggs") %>%
  select(year, age, treat_rate) %>%
  mutate(treat_rate = round(treat_rate, digits = 3)) %>%
  pivot_wider(names_from = age, values_from = treat_rate, names_prefix = "auto_age_")

data_fig1_donor <- data_fig1 %>%
  filter(egg_source == "(b) Donor eggs") %>%
  select(year, age, treat_rate) %>%
  mutate(treat_rate = round(treat_rate, digits = 3)) %>%
  pivot_wider(names_from = age, values_from = treat_rate, names_prefix = "donor_age_")

# merge the data sets
data_c1 <- merge(data_fig1_autologous, data_fig1_donor, by = "year")

############ share of donor treatments for first cycles ###################

# count treatments by age group and egg source over the whole time period,
# count total treatments, get share of egg donor treatments in each age group
data_share <- data_first %>%
  group_by(age, egg_source) %>%
  summarise(treatments = n()) %>%
  mutate(total = sum(treatments),
         share = round(treatments/total*100, digits = 1)) %>%
  filter(egg_source == 'Donor')

############## Figure 2: Success rates #################

# show success rates over time for all ages and success rates by
# age group for all years (shows both the improvement over time
# and the dependency on age)

# success rates by age group and year
collapsed_data <- data_first %>%
  group_by(age, year) %>%
  summarise(success = sum(live_birth == 1),
            failure = sum(live_birth == 0),
            .groups = 'drop') %>%
  mutate(sr_tot = success / (success + failure) * 100,
         treated = success + failure)

# get success rates by year
data_fig2_time <- collapsed_data %>%
  group_by(year) %>%
  summarise(success = sum(success),
            failure = sum(failure),
            treated = sum(treated)) %>%
  mutate(sr_tot = round(success/treated*100, digits = 1))

# save for appendix
data_c2 <- data_fig2_time %>%
  select(year, sr_tot) %>%
  mutate(sr_tot = sprintf("%.1f%%", sr_tot))

# get success rates by age group
data_fig2_age <- collapsed_data %>%
  group_by(age) %>%
  summarise(success = sum(success),
            failure = sum(failure),
            treated = sum(treated)) %>%
  mutate(sr_tot = round(success/treated*100, digits = 1))

# format for appendix
data_c3 <- data_fig2_age %>%
  select(age, sr_tot) %>%
  mutate(sr_tot = sprintf("%.1f%%", sr_tot))

# plot success rates over time
plot_time <- ggplot(data_fig2_time, aes(x = year, y = sr_tot)) +
  geom_line(size = 1.3, color = "darkgrey") +
  geom_point(size = 2.5, color = "darkgrey") +
  labs(title = "(a) Over time",
       x = "Year",
       y = "Successful cycles (percentage)") +
  ylim(0, 35) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"))

# plot success rates by age group
plot_age <- ggplot(data_fig2_age, aes(x = age, y = sr_tot, fill = as.factor(age))) +
  geom_bar(stat = "identity") +
  labs(title = "(b) By age",
       x = "Age",
       y = "Successful cycles (percentage)") +
  ylim(0, 35) +
  scale_fill_manual(values = colours_age) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Times New Roman"))

# combine the two plots into one layout
combined_plot <- plot_time + plot_age + plot_layout(ncol = 2) + 
  plot_annotation(title = "")

# display the combined plot
print(combined_plot)

############## Figure 3: Success rates over time #################

# calculate success rates by age, year, and egg source
data_success <- data_first %>%
  group_by(age, year, egg_source) %>%
  summarise(success = sum(live_birth == 1),
            failure = sum(live_birth == 0))%>%
  mutate(sr_tot= success/ (success+failure)*100,
         treated = success+failure) %>%
  arrange(age)

# aggregate success rates by period to reduce noise in egg donor treatment 
# success rates
data_fig3 <- data_success %>%
  mutate(period = case_when(
    year >= 1991 & year <= 1994 ~ "1991-94",
    year >= 1995 & year <= 1997 ~ "1995-97",
    year >= 1998 & year <= 2000 ~ "1998-2000",
    year >= 2001 & year <= 2003 ~ "2001-03",
    year >= 2004 & year <= 2006 ~ "2004-06",
    year >= 2007 & year <= 2009 ~ "2007-09",
    year >= 2010 & year <= 2012 ~ "2010-12",
    year >= 2013 & year <= 2015 ~ "2013-15",
    year >= 2016 ~ "2016-18"
  )) %>%
  group_by(period, age, egg_source) %>%
  summarise(success = sum(success),
            failure = sum(failure)) %>%
  mutate(treated = success + failure,
         sr_tot = success/treated*100,
         egg_source = factor(egg_source, levels = c("Patient", "Donor"),
                             labels = c("(a) Autologous eggs", "(b) Donor eggs")))

# plot
ggplot(data_fig3, aes(x = period, y = sr_tot, group = age, colour = age)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "",
    x = "Years", y = "Successful cycles (percentage)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  facet_wrap(~egg_source) +
  scale_color_manual(values = colours_age) +
  guides(color = guide_legend(title = "Age")) +
  theme(text = element_text(family = "Times New Roman"))

# bring the data in the format of the table for the appendix
data_fig3_autologous <- data_fig3 %>%
  filter(egg_source == "(a) Autologous eggs") %>%
  select(period, age, sr_tot) %>%
  mutate(sr_tot = round(sr_tot, digits = 1),
         sr_tot = sprintf("%.1f%%", sr_tot)) %>%
  pivot_wider(names_from = age, values_from = sr_tot, names_prefix = "auto_age_")

data_fig3_donor <- data_fig3 %>%
  filter(egg_source == "(b) Donor eggs") %>%
  select(period, age, sr_tot) %>%
  mutate(sr_tot = round(sr_tot, digits = 1),
         sr_tot = sprintf("%.1f%%", sr_tot)) %>%
  pivot_wider(names_from = age, values_from = sr_tot, names_prefix = "donor_age_")

# merge the data sets
data_c4 <- merge(data_fig3_autologous, data_fig3_donor, by = "period")

############## Figure 4: Sankey #################

# we want to show at what stage of the treatment patients fail in the oldest
# age group, separately by egg source

# there are some slightly weird things happening in the data, so have to add 
# some extra checks when counting:
# in the patient egg data, there are sometimes cycles with frozen eggs/embryos
# but num_eggs and num_embryos is counted as 0
# only count something as a failure if all subsequent steps have also
# failed, otherwise it will be a false positive

# summarise at what point first treatments fail for patients using
# donor eggs
data_fig4_donor <- data %>%
  filter(previous_cycles_ivf == 0,
         age == '45-50',
         donated_embryo == 0,
         egg_source == 'Donor',
         sperm_source == 'Partner') %>%
  summarise(
    total_treatments = n(),
    no_embryos = sum(num_embryos_created == 0 & total_embryos_transferred == 0 & pregnancy == 0),
    no_transfer = sum(num_embryos_created > 0 & total_embryos_transferred == 0 & pregnancy == 0),
    no_pregnancy = sum(total_embryos_transferred > 0 & pregnancy == 0 & live_birth == 0),
    no_live_birth = sum(pregnancy == 1 & live_birth == 0),
    live_birth = sum(live_birth == 1)
  )

# same for autologous eggs
data_fig4_patient <- data %>%
  filter(previous_cycles_ivf == 0,
         age == '45-50',
         donated_embryo == 0,
         egg_source == 'Patient',
         sperm_source == 'Partner') %>%
  summarise(
    total_treatments = n(),
    no_eggs = sum(num_eggs == 0 & num_embryos_created == 0 & total_embryos_transferred == 0 & pregnancy == 0 & live_birth == 0),
    no_embryos = sum(num_eggs != 0 & num_embryos_created == 0 & total_embryos_transferred == 0 & pregnancy == 0 & live_birth == 0),
    no_transfer = sum(num_embryos_created > 0 & total_embryos_transferred == 0 & pregnancy == 0 & live_birth == 0),
    no_pregnancy = sum(total_embryos_transferred > 0 & pregnancy == 0 & live_birth == 0),
    no_live_birth = sum(pregnancy == 1 & live_birth == 0),
    live_birth = sum(live_birth == 1)
  )


# Add success counts
data_fig4_patient <- data_fig4_patient %>%
  mutate(
    eggs_collected = total_treatments - no_eggs,
    embryos_created = eggs_collected - no_embryos,
    embryos_transferred = embryos_created - no_transfer,
    pregnancies = embryos_transferred - no_pregnancy,
    live_births = pregnancies - no_live_birth,
    test = live_births - live_birth
  )

data_fig4_donor <- data_fig4_donor %>%
  mutate(
    embryos_created = total_treatments - no_embryos,
    embryos_transferred = embryos_created - no_transfer,
    pregnancies = embryos_transferred - no_pregnancy,
    live_births = pregnancies - no_live_birth,
    test = live_births - live_birth
  )

# use sankeymatic to make sankey diagram from the success counts
# https://sankeymatic.com/build/

# sankey for treatments using autologous eggs is here:
# https://sankeymatic.com/build/?i=PTAEFEDsBcFMCdQDEA2B7A7gZ1AI1tBrLJKAHJoAmsWANKCgJYDWso0AFo1gFwBQIUEOHCAymgCu8AMZsA2gEEAsgHkAqmQAqAXVCaAhvADmBPn01po%2BlO3ix90ALYloOOQCYAbAAZvu8EZGONJoKCiw0nCU5pbWtvZOLm6e7gAcuhSgsIFYZgFBoCFhEVGgHu7uAJz%2BjrjwAJ5owXYOsNH5waHhkW1lAMwA7ACMGWhZtQ1NeRONzQm9Ht6pnjV1s7b6kFgAZgh27TNNhS2lckMALH2jG1u78NNrR9Dwmzt7C8O6AAp2RpCb0kYND44EOOGerzu%2BzK7m8QwArNcAA6%2Df6QaT1Mw%2DbJowE0MrndKgAAyjAAbmxcIx4Jxcti%2DgCgW53FdyGMmBS8NTOGZBABNSSFTagLAEUD6NnUADknXQ8HoTFY7C4vD4PEy2QKAGJKrq9WqNWDQDq9bqDWMIbcEMbTWb1WMUTiAfUbbbzQxyZTuRxXaazDwLFYbM8Es4YDgtQoAELeAaxtUdQpdEq9SMxuMDBNG6QnVPR2Pxnigx7gl5W%2BDQtMFzM8em4pnG%2DMZtWkzlUmkcCNN%2BMCMAiftCAB0w7QiAlRVHoG2k4lWEYkCM4WQ6Aw%2DBAUYklBM0DKQx8uhUnGtZAiNDn0AbWu8SG8vLAalFyrYIRg8FCOHwK%2DYY2kEiw0DQRxGAAL1gXtQEaKRQEoRh9CMF5HBlcUkSRewy1kYdBz4OdQNADBQCGXxbyEH0fFvRxDCMecGAI9w%2BCERAhjooQdyGVJ6LwUBYT4XAjCTOVjW2IThI4y0sCRQwXHIPhICoNh8KYjifXhYiRQkwEF1AAZ4Q43BR2oRBVKPZxxQ4idEB1IZKisoYOLQdTGGgF1bO2L9f3gMkHCkNhvEHHShHnDgEEc7Y30cdhKNMIRzN9fUhHs%2DRASc0BfPOHSUH0SCd30619AkACKIvaQOIAKz%2DC9tkaeBGCorZpKEMr%2D0YSqSEoHAyA4uwKXgUV4P0JEfQ6oQHCsaQuHRQCkXCOB%2DzGSA0JoaA%2BAyz9ZUnK8iN8JTGGoerQC4IwOCYQ6d187SOOnGBtkStgsFeABaUVqu2DimHm8TEvnPjfOY0A7Ayi8KRwtghkIjiKL%2BZrnK2lbYBQf4TP61DDBwPkOOBgjPA4ogao4Hdzhh%2DRP08lAJDYJG0NRi6JDCR1ATnNBSDRoQkSaRzGEZvA4cwbHYFx%2DHCc%2DVnzw50g8oA6watIVSsDG2BEfytALupf8uenOxdJaZhWfnHdMxJsmp1HQrQClWhBylDjHW2RgAA9TctoQsAkIS7YdvhjNgNAhNFHcJUzIRcBSszg6EShg%2BcKxQDDC9GbuyBWHqQrGGkPamCa2prABfE%2BSAA

# sankey for treatments using donor eggs is here:
# https://sankeymatic.com/build/?i=PTAEFEDsBcFMCdQDEA2B7A7gZ1AI1tBrLJKAHJoAmsWANKCgJYDWso0AFo1gFwBQIUEOHCAymgCu8AMZsA2gEEAsgHkAqmQAqAXVCaAhvADmBPn01po%2BlO3ix90ALYloOOQGYALAFYAnLoARNEg0RFgjIxwJLFhKMyCQsIiomMpQD3cAdnddcEdceABPNBxpOwdYvgTQ0HDI0GjY9IAOADZdClr8opKzPILi0vK4NI8ARk92iG7B231ILAAzBDs4%2Dp6h%2BxH0gCYdzI60OYXl%2BD6ZkuOllaa5MbHfb10ABTsjSHnpRho%2BddnoeDza7wVbpB7eMaHUAABzeH0g0kKZle4XhXxoYIADF5dAAZRgANzYuEY8E4WD4KPen2%2BbjGrUh5COTCJeFJnDMggAmpJQNJ5qAYtBQPomdQAOSlNDoeD0JisdhcXh8HidWAXHAAYl8Ot1Ks6AKBp1A2t1Ov1R1hqM%2BhRNZvNquZhOJ7I4dvtZh4FisNgBW2cMC1CgAQpjMmGVdUkvVGmlNSGwxGeH9LmUtk146Hw5kVSmcIaTjc4wnsyqqWjaSaS0n8aySWSOEGsxGBGA1DFFWxpMEAdKcPh0Bh2EdpNFoGhHIwAF6wVugYpSUCURj6IyAxySkXQ6H2QEI2AAOiPfCw07YQ7GmKvfCEbta18chiMjFINjGOxvoEQ78%2DwrGzU%2DXBQB2TE%2BFwIw%2BWlGpNUWWC4N%2DPcsGhQwXHIPgQmoUALw%2DW9QG8UChCQ%2DQvkgCDMm8QDQmoRACM7ZwRU%2DbsZRNZpWLYz80GQr5oFtMY%2BEWQc%2BSkAkHCkNhMQPCihBfDgEEYaBFngCd2CfUwhCY6D7V8DiuPk20JJ8PgUH0BdhSohARQkcdH2gRhpE%2DAArMdGEWYp4EYZ8FjQoQnKwWzXJISgcDIT87CJeAYjXfRoTdEKhAcKxpC4BEJ2hFACBocdQEgXdMqM%2DQBylZjNSvUraK4TC4tALgjA4JhauFCTyM%2DRYe0WYi2CwIEAFoYncxZPyYHKiJIiCJJwr9YGM2yiVPGdQHuWjH3eFzeOvYyBw%2Bejop3QwcC5T85rYelPyIDyOGFTx1oKqaRJQCQ2B23d9paiQUBQK0vlPYJQAOoRoRKeTGB%2BgdMFO2Bzsu66BwB09bJ%2B%2DQrLQawPNIWisCS9VHqRlrST8vBYFauxAPKZgAZfYUczuh7QCJmzQHFWgD3FT8rUWRgAA8GZZwiJFgznub4TgsbQWChRFUBtKEIDaOkUBaLSUDnCsUAA3hhZ5lYQobLs7yGG4Wz8msT4MS5IA

############## Table 2: TFR contribution #################

# want the data with birth year now instead of treatment year
data_by <- data %>%
  rename(year_treatment = year,
         year = year_birth) %>%
  filter(live_birth == 1,
         year %in% c(1992:2018))

# count art births by year, age, and egg source
data_art_births <- data_by %>%
  group_by(year, age, egg_source) %>%
  summarise(art_births = sum(num_live_births)) %>%
  pivot_wider(
    names_from = egg_source,
    values_from = art_births,
    names_prefix = "",
    values_fill = 0  # this will fill NAs with 0
  ) %>%
  rename(patient_births = Patient, donor_births = Donor) %>%
  mutate(art_births = patient_births + donor_births)

# join with population to calculate asfr and tfr
# to calculate tfr, need to weigh asfr by number of years in each age group,
# then sum up
data_art <- left_join(data_art_births, data_exp) %>%
  mutate(asfr = art_births/population,
         asfr_weighed = asfr * num_years,
         asfr_donor = donor_births/population,
         asfr_donor_weighed = asfr_donor * num_years) %>%
  group_by(year) %>%
  summarise(art_tfr = sum(asfr_weighed),
            donor_tfr = sum(asfr_donor_weighed))

# join with HFD data on the TFR and calculate non-ART tfr
data_all_tfr <- left_join(data_art, data_TFR) %>% 
  mutate(non_art_tfr = tfr - art_tfr,
         percent_art = art_tfr / tfr * 100,
         percent_donor = donor_tfr / art_tfr * 100,
         tfr = round(tfr, digits = 2),
         art_tfr = round(art_tfr, digits = 3),
         percent_donor = round(percent_donor, digits = 1),
         percent_art = round(percent_art, digits = 1))

# select the relevant data for Table 2
data_tab2 <- data_all_tfr %>%
  mutate(percent_art = round(art_tfr/tfr*100, digits = 1)) %>%
  select(year, tfr, art_tfr, percent_art, percent_donor) %>%
  filter(year %in% c(1992, 1995, 2000, 2005, 2010, 2015, 2018))

############## Figure 5: ASFR contribution #################

# need to use birth year instead of treatment year as for table 2

# summarise ART births by year, age, and egg source and
# add total births from HFD
# reorder the factor levels for plotting
data_fig5 <- data_by %>%
  select(year, age, num_live_births, egg_source) %>%
  group_by(year, age, egg_source) %>%
  summarise(art_births = sum(num_live_births), .groups = 'drop') %>%
  left_join(data_births, by = c("year", "age")) %>%
  mutate(percent_art = art_births / total_births * 100,
         egg_source = factor(egg_source, levels = c("Donor", "Patient")))

# bring the data in the format of the table for the appendix
fig5_own <- data_fig5 %>%
  filter(egg_source == 'Patient') %>%
  select(year, age, percent_art) %>%
  mutate(percent_art = round(percent_art, digits = 2),
         percent_art = sprintf("%.2f%%", percent_art)) %>%
  pivot_wider(names_from = age, values_from = percent_art, names_prefix = "auto_age_") %>%
  mutate(`auto_age_45-50` = replace_na(`auto_age_45-50`, "0.00%"))

fig5_donor <- data_fig5 %>%
  filter(egg_source == 'Donor') %>%
  select(year, age, percent_art) %>%
  mutate(percent_art = round(percent_art, digits = 2),
         percent_art = sprintf("%.2f%%", percent_art)) %>%
  pivot_wider(names_from = age, values_from = percent_art, names_prefix = "donor_age_")

# merge the data sets
data_c5 <- merge(fig5_own, fig5_donor, by = "year")

# plot
ggplot(data = data_fig5, aes(x = year, y = percent_art, fill = egg_source)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  facet_wrap(~age) +
  labs(title = "",
       fill = "Egg source", x = "Year", y = "Share of ASFR (percentage)") +
  scale_fill_manual(values = c("Patient" = "gray80", "Donor" = "gray30")) +
  theme(legend.position = "bottom") +
  theme(text = element_text(family = "Times New Roman"))

# percent donor out of all ART births
data_share_births <- data_by %>%
  select(year, age, num_live_births, egg_source) %>%
  group_by(year, age, egg_source) %>%
  summarise(art_births = sum(num_live_births), .groups = 'drop') %>%
  pivot_wider(
    names_from = egg_source,
    values_from = art_births,
    names_prefix = "",
    names_sep = "_",
    values_fill = 0
  ) %>%
  rename(
    patient_births = Patient,
    donor_births = Donor
  ) %>%
  mutate(total = patient_births + donor_births,
         share_donor = round(donor_births/total*100, digits = 1))

# percent ASFR/births contribution of all treatments
data_asfr_total <- data_fig5 %>%
  group_by(year, age) %>%
  summarise(percent_art = sum(percent_art))

################## mutliple births ###################

# show that multiple births have been going down and that therefore
# the increased contribution is despite fewer babies per successful treatment

# count successful cycles and number of live births in each year, calculate ratio
data_multiples <- data_by %>%
  group_by(year) %>%
  summarise(art_births = sum(num_live_births),
            successful_treatments = n()) %>%
  mutate(births_per_success = art_births/successful_treatments)

ggplot(data_multiples, aes(x = year, y = births_per_success)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Year",
       y = "Births per successful treatment",
       title = "") +
  theme_minimal() +
  ylim(1, 1.4) +
  theme(text = element_text(family = "Times New Roman"))

############## Figure 6: Absolute ART ASFR #################

# summarise ART births by year, age, and egg_source and
# add population exposure from HFD
# again, need to use data by birth year here

data_asfr <- data_by %>%
  select(year, age, num_live_births) %>%
  group_by(year, age) %>%
  summarise(art_births = sum(num_live_births), .groups = 'drop') %>%
  left_join(data_exp, by = c("year", "age")) %>%
  mutate(asfr = art_births / population * 1000)

ggplot(data_asfr, aes(x = year, y = asfr, group = age, colour = age)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "",
    x = "Year", y = "ART births per 1,000 women",
    colour = "Age"
  ) +
  theme_minimal() +
  scale_color_manual(values = colours_age) +
  scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015)) +
  theme(text = element_text(family = "Times New Roman"))

# bring the data in the format of the table for the appendix
data_c6 <- data_asfr %>%
  select(year, age, asfr) %>%
  mutate(asfr = round(asfr, digits = 2)) %>%
  pivot_wider(names_from = age, values_from = asfr)

################### Appendix A: cycle rank ######################

# calculate success rates for all cycles by not filtering for the first cycle
# as before we do this by period to reduce noise
data_all_cycles <- data %>%
  mutate(period = case_when(
    year >= 1991 & year <= 1994 ~ "1991-94",
    year >= 1995 & year <= 1997 ~ "1995-97",
    year >= 1998 & year <= 2000 ~ "1998-2000",
    year >= 2001 & year <= 2003 ~ "2001-03",
    year >= 2004 & year <= 2006 ~ "2004-06",
    year >= 2007 & year <= 2009 ~ "2007-09",
    year >= 2010 & year <= 2012 ~ "2010-12",
    year >= 2013 & year <= 2015 ~ "2013-15",
    year >= 2016 ~ "2016-18"
  )) %>%
  group_by(period, age, egg_source) %>%
  summarise(
    success = sum(live_birth == 1),
    failure = sum(live_birth == 0),
    .groups = 'drop'
  ) %>%
  mutate(
    treated = success + failure,
    sr_tot = success / treated * 100,
    success_rate = success / (success + failure) * 100,
    egg_source = factor(egg_source, 
                        levels = c("Patient", "Donor"),
                        labels = c("(a) Autologous eggs", "(b) Donor eggs"))
  )

# plot
ggplot(data_all_cycles, aes(x = period, y = sr_tot, group = age, colour = age)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "",
    x = "Years", 
    y = "Successful cycles (percentage)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  facet_grid(~ egg_source) +
  scale_color_manual(values = colours_age) +
  guides(color = guide_legend(title = "Age")) +
  theme(text = element_text(family = "Times New Roman"))

# now filter for the first three cycles and compare the success rates by cycle rank
data_cycles <- data %>%
  filter(previous_cycles_ivf %in% c(0, 1, 2)) %>%
  mutate(cycle = paste0("Cycle ", previous_cycles_ivf + 1))

# calculate success rates by cycle rank
data_figa1 <- data_cycles %>%
  mutate(period = case_when(
    year >= 1991 & year <= 1994 ~ "1991-94",
    year >= 1995 & year <= 1997 ~ "1995-97",
    year >= 1998 & year <= 2000 ~ "1998-2000",
    year >= 2001 & year <= 2003 ~ "2001-03",
    year >= 2004 & year <= 2006 ~ "2004-06",
    year >= 2007 & year <= 2009 ~ "2007-09",
    year >= 2010 & year <= 2012 ~ "2010-12",
    year >= 2013 & year <= 2015 ~ "2013-15",
    year >= 2016 ~ "2016-18"
  )) %>%
  group_by(period, age, egg_source, cycle) %>%
  summarise(
    success = sum(live_birth == 1),
    failure = sum(live_birth == 0),
    .groups = 'drop'
  ) %>%
  mutate(
    treated = success + failure,
    sr_tot = success / treated * 100,
    success_rate = success / (success + failure) * 100,
    egg_source = factor(egg_source, 
                        levels = c("Patient", "Donor"),
                        labels = c("(a) Autologous eggs", "(b) Donor eggs"))
  )

# add an "age x cycle" variable to be able to use linetypes
data_figa1 <- data_figa1 %>%
  mutate(age_cycle = interaction(age, cycle, sep = "_"))

# plot
ggplot(data_figa1, aes(x = period, y = sr_tot, 
                       group = age_cycle, 
                       colour = age, 
                       linetype = cycle)) +
  geom_line(size = 1) +
  labs(
    title = "",
    x = "Years", 
    y = "Successful cycles (percentage)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10),
    legend.box = "vertical",
    legend.spacing.y = unit(0.3, "cm"),
    legend.key.width = unit(1, "cm")
  ) +
  facet_wrap(~ egg_source) +
  scale_color_manual(values = colours_age) +
  scale_linetype_manual(values = c("Cycle 1" = "solid", 
                                   "Cycle 2" = "dashed", 
                                   "Cycle 3" = "dotted")) +
  guides(
    color = guide_legend(title = "Age", order = 1),
    linetype = guide_legend(title = "Cycle rank", order = 2)
  ) +
  theme(text = element_text(family = "Times New Roman"))

################### Appendix B: male age ######################

## first, look at partner age ##

# get 2017-2018 data, other years do not have partner age
data_partner <- data_first %>%
  filter(year %in% c(2017, 2018),
         sperm_source == 'Partner',
         !(age_partner %in% c("", "999")))

# calculate success rates
data_success_partner <- data_partner %>%
  group_by(age, age_partner) %>%
  summarise(success = sum(live_birth == 1),
            failure = sum(live_birth == 0),
            .groups = 'drop') %>%
  mutate(sr_tot = success / (success + failure) * 100,
         treated = success + failure)

# order age_partner correctly
age_partner_order <- c("18-34", "35-37", "38-39", "40-42", "43-44", "45-50", "51-55", "56-60", ">60")
data_success_partner$age_partner <- factor(data_success_partner$age_partner, levels = age_partner_order)

# plot
ggplot(data_success_partner, aes(x = age_partner, y = sr_tot, color = as.factor(age), group = as.factor(age))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colours_age, name = "Age of female patient") +
  labs(x = "Age of male partner",
       y = "Successful cycles (percentage)",
       title = "") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Added angled labels for better readability
  ) +
  theme(text = element_text(family = "Times New Roman"))

# generate a table with the sample sizes
data_b1 <- data_success_partner %>%
  select(age, age_partner, treated) %>%
  pivot_wider(names_from = age_partner, 
              values_from = treated)

## Second, look at sperm donor age ##

# make a vector with the correct age ordering for sperm donors
age_order <- c("<=20", "21-25", "26-30", "31-35", "36-40", "41-45", ">45")

# calculate success rates by female patient age and sperm donor age
data_sperm_donor <- data %>%
  filter(sperm_source == 'Donor',
         previous_cycles_ivf == 0) %>%
  group_by(age, sperm_donor_age) %>%
  summarise(success = sum(live_birth == 1),
            failure = sum(live_birth == 0),
            .groups = 'drop') %>%
  mutate(sr_tot = success / (success + failure) * 100,
         treated = success + failure) %>%
  filter(!is.na(sperm_donor_age) & sperm_donor_age != '999') %>%
  mutate(sperm_donor_age = factor(sperm_donor_age, levels = age_order))

# plot
ggplot(data_sperm_donor, aes(x = sperm_donor_age, y = sr_tot, color = as.factor(age), group = as.factor(age))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colours_age, name = "Age of female patient") +
  labs(x = "Age of sperm donor",
       y = "Successful cycles (percentage)",
       title = "") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Added angled labels for better readability
  ) +
  theme(text = element_text(family = "Times New Roman"))

# generate a table with the sample sizes
data_b2 <- data_sperm_donor %>%
  select(age, sperm_donor_age, treated) %>%
  pivot_wider(names_from = sperm_donor_age, 
              values_from = treated) %>%
  relocate(age, `<=20`)
