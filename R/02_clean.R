# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)


install.packages("tibble")
install.packages("readr")

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
                                    status == "dead - respiratory disease" ~ "respiratory disease")) 

#Remove columns
prostate_data_clean$rx <- NULL
prostate_data_clean$status <- NULL

<<<<<<< HEAD
#variables to be changed- rx--> placebo, 
#recent MIS, 
install.packages("chron")
library(chron)
 as.Date.numeric(prostate$sdate, origin())
?as.Date

 
 
=======
>>>>>>> aa1ac954e1f2603a4bc95ddea636745a15b3d614
