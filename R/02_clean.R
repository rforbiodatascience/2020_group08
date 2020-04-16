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

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "Data/01_prostate_data.tsv") %>% 
  as_tibble()

# Wrangle data
# ------------------------------------------------------------------------------

#Add columns
prostate_data_clean <- 
  prostate_data %>%
      mutate(estrogen_mg = case_when(rx == "0.2 mg estrogen" ~ 0.2,
                                 rx == "1.0 mg estrogen" ~ 1,
                                 rx == "5.0 mg estrogen" ~ 5,
                                 rx == "placebo" ~ 0),
         status_ = case_when(status == "alive" ~ "alive",
                             str_detect(status, "dead") ~ "dead"),
         cause_of_death = case_when(status == "dead - other ca" ~ "other cancer",
                                    status == "dead - cerebrovascular" ~ "cerebrovascular disease/accident",
                                    status == "dead - prostatic ca" ~ "prostate cancer",
                                    status == "dead - heart or vascular" ~ "heart or vascular disease",
                                    status == "alive" ~ "N/A",
                                    status == "dead - pulmonary embolus" ~ "pulmonary embolus",
                                    status == "dead - other specific non-ca" ~ "other specified non-cancer",
                                    status == "dead - unknown cause" ~ "unknown",
                                    status == "dead - unspecified non-ca" ~ "unspecified non-cancer",
                                    status == "dead - respiratory disease" ~ "respiratory disease"),
         dead_from_prostate_cancer = case_when(status == "dead - other ca" ~ 0,
                                               status == "dead - cerebrovascular" ~ 0,
                                               status == "dead - prostatic ca" ~ 1,
                                               status == "dead - heart or vascular" ~ 0,
                                               status == "alive" ~ 2,
                                               status == "dead - pulmonary embolus" ~ 0,
                                               status == "dead - other specific non-ca" ~ 0,
                                               status == "dead - unknown cause" ~ 0,
                                               status == "dead - unspecified non-ca" ~ 0,
                                               status == "dead - respiratory disease" ~ 0),
         Age_group = case_when(45 <= age & age < 60 ~ "45 - 59",
                               60 <= age & age < 70 ~ "60 - 69",
                               70 <= age & age < 80 ~ "70 - 79",
                               80 <= age & age < 90 ~ "80 - 90")) %>%
                                
  na_if("N/A")


#Remove columns
prostate_data_clean$rx <- NULL
prostate_data_clean$status <- NULL

#Rename columns
prostate_data_clean <- prostate_data_clean %>% 
  rename(months_of_follow_up = dtime, weight_index = wt, activity = pf, history_of_CD = hx,
         serum_hemoglobin = hg,tumor_size = sz, stage_grade_index = sg, PA_phosphatase = ap, 
         bone_metastases = bm, systolic_bp = sbp, diastolic_bp = dbp)

#Convert value to NA, which was wrongly assigned a value according to the authors of the study
#http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/prostate.notes.txt
prostate_data_clean <- na_if(prostate_data_clean, 999.87500000)

#Ensure that factorial variables are actually factors
factor_columns <- c("stage", "activity", "history_of_CD", "ekg", "bone_metastases",
                    "estrogen_mg", "status_", "cause_of_death", "dead_from_prostate_cancer",
                    "Age_group")

prostate_data_clean[factor_columns] <- lapply(prostate_data_clean[factor_columns], factor)

prostate_data_clean

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_data_clean,
          path = "Data/02_prostate_data_clean.tsv")
