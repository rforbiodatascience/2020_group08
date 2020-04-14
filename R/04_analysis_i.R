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
  geom_point(alpha = 0.4) +
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

### Different approach --> facet_wrap on character variables
pdc_long_characterColumn <- prostate_data_clean %>% 
  pivot_longer(cols = c(activity, ekg, status_, cause_of_death),
               names_to = "variable",
               values_to = "value")

pdc_long_characterColumn %>% 
  ggplot(aes(x = estrogen_mg, y = age, colour = value)) +
  geom_jitter() +
  facet_wrap(~ variable, ncol = 2)

### Different approach 

#Estrogen vs age
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = age, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = age, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = age, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = age, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs stage
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = stage, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = stage, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = stage, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = stage, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs follow_up
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = months_of_follow_up, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = months_of_follow_up, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = months_of_follow_up, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = months_of_follow_up, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs weight_index
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = weight_index, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = weight_index, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = weight_index, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = weight_index, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs history of CD
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = history_of_CD, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = history_of_CD, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = history_of_CD, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = history_of_CD, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs systolic bp
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = systolic_bp, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = systolic_bp, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = systolic_bp, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = systolic_bp, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs diastolic bp
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = diastolic_bp, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = diastolic_bp, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = diastolic_bp, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = diastolic_bp, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs serum hemoglobin
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = serum_hemoglobin, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = serum_hemoglobin, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = serum_hemoglobin, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = serum_hemoglobin, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs tumor size
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = tumor_size, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = tumor_size, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = tumor_size, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = tumor_size, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs stage grade index
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = stage_grade_index, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = stage_grade_index, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = stage_grade_index, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = stage_grade_index, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs PA phosphatase
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = PA_phosphatase, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = PA_phosphatase, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = PA_phosphatase, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = PA_phosphatase, colour = cause_of_death)) +
  geom_jitter()

#Estrogen vs bone metastases
prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = bone_metastases, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = bone_metastases, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = bone_metastases, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = estrogen_mg, y = bone_metastases, colour = cause_of_death)) +
  geom_jitter()

#######

#PA_phosphatase vs age
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = age, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = age, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = age, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = age, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs stage
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = stage, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = stage, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = stage, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = stage, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs follow_up
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = months_of_follow_up, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = months_of_follow_up, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = months_of_follow_up, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = months_of_follow_up, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs weight_index
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = weight_index, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = weight_index, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = weight_index, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = weight_index, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs history of CD
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = history_of_CD, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = history_of_CD, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = history_of_CD, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = history_of_CD, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs systolic bp
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = systolic_bp, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = systolic_bp, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = systolic_bp, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = systolic_bp, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs diastolic bp
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = diastolic_bp, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = diastolic_bp, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = diastolic_bp, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = diastolic_bp, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs serum hemoglobin
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = serum_hemoglobin, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = serum_hemoglobin, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = serum_hemoglobin, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = serum_hemoglobin, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs tumor size
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = tumor_size, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = tumor_size, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = tumor_size, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = tumor_size, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs stage grade index
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = stage_grade_index, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = stage_grade_index, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = stage_grade_index, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = stage_grade_index, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs estrogen
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = estrogen_mg, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = estrogen_mg, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = estrogen_mg, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = estrogen_mg, colour = cause_of_death)) +
  geom_jitter()

#PA_phosphatase vs bone metastases
prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = bone_metastases, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = bone_metastases, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = bone_metastases, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = PA_phosphatase, y = bone_metastases, colour = cause_of_death)) +
  geom_jitter()


########

#bone_metastases vs age
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = age, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = age, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = age, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = age, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs stage
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = stage, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = stage, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = stage, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = stage, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs follow_up
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = months_of_follow_up, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = months_of_follow_up, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = months_of_follow_up, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = months_of_follow_up, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs weight_index
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = weight_index, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = weight_index, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = weight_index, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = weight_index, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs history of CD
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = history_of_CD, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = history_of_CD, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = history_of_CD, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = history_of_CD, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs systolic bp
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = systolic_bp, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = systolic_bp, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = systolic_bp, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = systolic_bp, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs diastolic bp
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = diastolic_bp, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = diastolic_bp, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = diastolic_bp, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = diastolic_bp, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs serum hemoglobin
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = serum_hemoglobin, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = serum_hemoglobin, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = serum_hemoglobin, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = serum_hemoglobin, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs tumor size
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = tumor_size, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = tumor_size, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = tumor_size, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = tumor_size, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs stage grade index
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = stage_grade_index, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = stage_grade_index, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = stage_grade_index, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = stage_grade_index, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs estrogen
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = estrogen_mg, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = estrogen_mg, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = estrogen_mg, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = estrogen_mg, colour = cause_of_death)) +
  geom_jitter()

#bone_metastases vs PA phosphatase
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = PA_phosphatase, colour = status_)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = PA_phosphatase, colour = ekg)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = PA_phosphatase, colour = activity)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = PA_phosphatase, colour = cause_of_death)) +
  geom_jitter()

prostate_data_clean %>% 
  ggplot(aes(x = diastolic_bp, y = systolic_bp, color = history_of_CD)) +
  geom_jitter()

# Potential correlations:
# PA phosphatase + cause of death
# Bone metastases + status
# Bone metastases + activity (maybe?)
# Stage/grade index + activity
# Stage/grade index + PA phosphatase (stratified on status_ and activity)
# Bone metastases + PA phosphatase (stratified on cause of death) --> boxplot
# Tumor size + PA phosphatase
# Bone metastases + age (stratified on status and activity and cause of death)
# Stage + bone metastases (stratified on activity and cause of death)
# Weight index + bone metastases --> e.g make boxplot
# History of CD + bone metastases (stratified on status)
# Serum hemoglbin + bone metastases --> e.g. boxplot
# Tumor size + bone metastases --> e.g. boxplot
# Stage grade index + bone metastases (stratified on activity and cause of death)



##### Nesting data frames for modelling: #########


# Diastolic vs systolic bp

bp_model <- function(prostate_data_clean) {
  lm(systolic_bp ~ diastolic_bp, data = prostate_data_clean)
}

cause_of_death_nest <- prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model = map(data, bp_model))

bp_glance <- cause_of_death_nest %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance)



