# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())


# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(tibble)
library(stringr)
library(readr)


# Load data
# ------------------------------------------------------------------------------
prostate_data_clean <- read_tsv(file = "Data/02_prostate_data_clean.tsv") %>% 
  as_tibble()


# Wrangle data
# ------------------------------------------------------------------------------

prostate_data_clean_aug <- prostate_data_clean %>% 
  mutate(dead_from_prostate_cancer = case_when(status == "dead - other ca" ~ 0,
                                               status == "dead - cerebrovascular" ~ 0,
                                               status == "dead - prostatic ca" ~ 1,
                                               status == "dead - heart or vascular" ~ 0,
                                               status == "alive" ~ 99,
                                               status == "dead - pulmonary embolus" ~ 0,
                                               status == "dead - other specific non-ca" ~ 0,
                                               status == "dead - unknown cause" ~ 0,
                                               status == "dead - unspecified non-ca" ~ 0,
                                               status == "dead - respiratory disease" ~ 0),
         Age_group = case_when(45 <= age & age < 60 ~ "45 - 59",
                               60 <= age & age < 70 ~ "60 - 69",
                               70 <= age & age < 80 ~ "70 - 79",
                               80 <= age & age < 90 ~ "80 - 90")) %>% 
  na_if(99)

# Remove column
prostate_data_clean_aug$status <- NULL


# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_data_clean_aug,
          path = "Data/03_prostate_data_clean_aug.tsv")
