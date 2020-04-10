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

# Wrangle and analyse data
# ------------------------------------------------------------------------------

#Plots for at quick overview

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



# Looking at the bone_metastases variable (some interesting trends here)
pdc_long_BoneMetastases <- prostate_data_clean %>% 
  pivot_longer(-c(patno, bone_metastases, activity, status_, cause_of_death, ekg, sdate),
               names_to = "variable",
               values_to = "value")

pdc_long_BoneMetastases %>% 
  ggplot(aes(y = bone_metastases, x = value, color = activity)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_BoneMetastases %>% 
  ggplot(aes(y = bone_metastases, x = value, color = ekg)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_BoneMetastases %>% 
  ggplot(aes(y = bone_metastases, x = value, color = status_)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_BoneMetastases %>% 
  ggplot(aes(y = bone_metastases, x = value, color = cause_of_death)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)



# Looking at the estrogen variable
pdc_long_estrogen <- prostate_data_clean %>% 
  pivot_longer(-c(patno, estrogen_mg, activity, status_, cause_of_death, ekg, sdate),
               names_to = "variable",
               values_to = "value")

pdc_long_estrogen %>% 
  ggplot(aes(x = estrogen_mg, y = value, color = activity)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_estrogen %>% 
  ggplot(aes(x = estrogen_mg, y = value, color = ekg)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_estrogen %>% 
  ggplot(aes(x = estrogen_mg, y = value, color = status_)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_estrogen %>% 
  ggplot(aes(x = estrogen_mg, y = value, color = cause_of_death)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

