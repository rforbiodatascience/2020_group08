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
                                    status == "dead - respiratory disease" ~ "respiratory disease")) %>% 
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
prostate_data_clean <- na_if(prostate_data_clean, 999.87500000)

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_data_clean,
          path = "Data/02_prostate_data_clean.tsv")
