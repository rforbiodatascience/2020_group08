# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(tibble)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "Data/02_prostate_data_clean.tsv") %>% 
  as_tibble()


# Boxplots
# ------------------------------------------------------------------------------


# Scatter plots
# ------------------------------------------------------------------------------


# Wrangle data
# ------------------------------------------------------------------------------

# Wrangle to long format omitting systolic_bp, since this variable is investigated
# All character columns are also omitted as these are used for stratification
pdc_long_systolic_bp <- prostate_data %>% 
  pivot_longer(-c(patno, systolic_bp, activity, status_, cause_of_death, ekg, sdate),
               names_to = "variable",
               values_to = "value")

pdc_long_systolic_bp %>% 
  ggplot(aes(y = systolic_bp, x = value, color = activity)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_systolic_bp %>% 
  ggplot(aes(y = systolic_bp, x = value, color = ekg)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_systolic_bp %>% 
  ggplot(aes(y = systolic_bp, x = value, color = status_)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_systolic_bp %>% 
  ggplot(aes(y = systolic_bp, x = value, color = cause_of_death)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)


# Wrangle to long format omitting systolic_bp, since this variable is investigated
# All character columns are also omitted as these are used for stratification
pdc_long_diastolic_bp <- prostate_data %>% 
  pivot_longer(-c(patno, diastolic_bp, activity, status_, cause_of_death, ekg, sdate),
               names_to = "variable",
               values_to = "value")

pdc_long_diastolic_bp %>% 
  ggplot(aes(y = diastolic_bp, x = value, color = activity)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_diastolic_bp %>% 
  ggplot(aes(y = diastolic_bp, x = value, color = ekg)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_diastolic_bp %>% 
  ggplot(aes(y = diastolic_bp, x = value, color = status_)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_diastolic_bp %>% 
  ggplot(aes(y = diastolic_bp, x = value, color = cause_of_death)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

