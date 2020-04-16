# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(forcats)

# Load data
# ------------------------------------------------------------------------------
prostate_data_clean <- read_tsv(file = "Data/02_prostate_data_clean.tsv")

#Ensure that factorial variables are actually factors
factor_columns <- c("stage", "activity", "history_of_CD", "ekg", "bone_metastases",
                    "estrogen_mg", "status_", "cause_of_death", "dead_from_prostate_cancer",
                    "Age_group")

prostate_data_clean[factor_columns] <- lapply(prostate_data_clean[factor_columns], factor)

# Wrangle and analyse data
# ------------------------------------------------------------------------------

##### Nesting data frames for modelling: #########


# Diastolic vs systolic bp grouped by cause_of_death

bp_model <- function(prostate_data_clean) {
  lm(systolic_bp ~ diastolic_bp, data = prostate_data_clean)
}

cause_of_death_nest <- prostate_data_clean %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model = map(data, bp_model))

bp_cause_glance <- cause_of_death_nest %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance)

prostate_data_clean %>% 
  ggplot(aes(x = diastolic_bp, y = systolic_bp)) +
  geom_jitter() + 
  geom_smooth(method = lm, se = T ) + 
  facet_wrap(~ cause_of_death)


###### Plots #######

### PA phosphatase + cause of death

prostate_data_clean %>% 
  na.omit() %>% 
  ggplot(aes(x = fct_reorder(cause_of_death, PA_phosphatase, .fun = mean, .desc = T), y = PA_phosphatase, color = bone_metastases)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Cause of death",
       y = "Serum Prostatic Acid Phosphatase (unit?)")

### Bone metastases + status
# Calculate "dead from prostate cancer" percentage
metastases_cancer_dead_percentage <- (prostate_data_clean %>% 
                                        filter(status_ == "dead", bone_metastases == "1", dead_from_prostate_cancer == 1) %>% 
                                        nrow())/
  ((prostate_data_clean %>% 
      filter(status_ == "dead", bone_metastases == "1", dead_from_prostate_cancer == 1) %>% 
      nrow())+
     (prostate_data_clean %>% 
        filter(status_ == "dead", bone_metastases == "1", dead_from_prostate_cancer == 0) %>% 
        nrow())) * 100

metastases_cancer_dead_percentage <- str_c(round(metastases_cancer_dead_percentage, 2), "%")


nonmetastases_cancer_dead_percentage <- (prostate_data_clean %>% 
                                           filter(status_ == "dead", bone_metastases == "0", dead_from_prostate_cancer == 1) %>% 
                                           nrow())/
  ((prostate_data_clean %>% 
      filter(status_ == "dead", bone_metastases == "0", dead_from_prostate_cancer == 1) %>% 
      nrow())+
     (prostate_data_clean %>% 
        filter(status_ == "dead", bone_metastases == "0", dead_from_prostate_cancer == 0) %>% 
        nrow())) * 100

nonmetastases_cancer_dead_percentage <- str_c(round(nonmetastases_cancer_dead_percentage, 2), "%")

#plot
prostate_data_clean %>% 
  ggplot(aes(x = bone_metastases, y = status_, color = dead_from_prostate_cancer)) +
  geom_jitter() + 
  annotate("text", x = 1:2, y = 2.5, label = c(nonmetastases_cancer_dead_percentage, metastases_cancer_dead_percentage)) +
  labs(x = "Bone metastases",
       y = "Status")


# Bone metastases + activity

# Transform count by group
activity_percentage <- prostate_data_clean %>% 
  group_by(activity, bone_metastases)%>% 
  summarise(counts = n()) %>% 
  pivot_wider(
    names_from = bone_metastases,
    values_from = counts) %>% 
  rename(. ,no_meta = "0", meta = "1")

# replace NA's with 0
activity_percentage[is.na(activity_percentage)] <- 0  

# calculate percentage and plot 
activity_percentage %>% 
  mutate(
    no = no_meta/sum(activity_percentage$no_meta) * 100,
    yes = meta/sum(activity_percentage$meta) * 100
  ) %>% 
  pivot_longer(
    -c(activity, meta, no_meta),
    names_to = "bone_metastases",
    values_to = "percentage"
  ) %>% 
  ggplot(aes(x = activity, y = percentage, fill = bone_metastases)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
