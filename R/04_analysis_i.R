# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)

# Load data
# ------------------------------------------------------------------------------
prostate_data_clean <- read_tsv(file = "Data/02_prostate_data_clean.tsv")

# Wrangle data
# ------------------------------------------------------------------------------

# Looking at the PA_phosphatase variable
# Wrangle to long format omitting PA_phospahtase, since this variable is investigated
# All character columns are also omitted as these are used for stratification
pdc_long_PAphosphatase <- prostate_data_clean %>% 
  pivot_longer(-c(patno, PA_phosphatase, activity, status_, cause_of_death, ekg, sdate),
               names_to = "variable",
               values_to = "value")

pdc_long_PAphosphatase %>% 
  ggplot(aes(y = PA_phosphatase, x = value, color = activity)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)
  
pdc_long_PAphosphatase %>% 
  ggplot(aes(y = PA_phosphatase, x = value, color = ekg)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_PAphosphatase %>% 
  ggplot(aes(y = PA_phosphatase, x = value, color = status_)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_PAphosphatase %>% 
  ggplot(aes(y = PA_phosphatase, x = value, color = cause_of_death)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)
