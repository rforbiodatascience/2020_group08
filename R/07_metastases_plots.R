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
prostate_data_clean_aug <- read_tsv(file = "Data/03_prostate_data_clean_aug.tsv")

#Ensure that factorial variables are actually factors
factor_columns <- c("stage", "activity", "history_of_CD", "ekg", "bone_metastases",
                    "estrogen_mg", "status_", "cause_of_death", "dead_from_prostate_cancer",
                    "Age_group")

prostate_data_clean_aug[factor_columns] <- lapply(prostate_data_clean_aug[factor_columns], factor)

# Wrangle and analyse data
# ------------------------------------------------------------------------------

## 1 - PA phosphatase vs cause of death stratified by bone metastases
PA_death_plot <- prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(x = fct_reorder(cause_of_death, PA_phosphatase, .fun = mean, .desc = T), y = PA_phosphatase, color = bone_metastases)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Cause of death",
       y = "Prostatic acid phosphatase level")


## 2 - Bone metastases vs status stratified by dead_from_proste_cancer

# Calculate "dead from prostate cancer" percentage for bone metastases
metastases_cancer_dead_percentage <- (prostate_data_clean_aug %>% 
                                        filter(status_ == "dead", bone_metastases == "1", dead_from_prostate_cancer == 1) %>% 
                                        nrow())/
  ((prostate_data_clean_aug %>% 
      filter(status_ == "dead", bone_metastases == "1", dead_from_prostate_cancer == 1) %>% 
      nrow())+
     (prostate_data_clean_aug %>% 
        filter(status_ == "dead", bone_metastases == "1", dead_from_prostate_cancer == 0) %>% 
        nrow())) * 100

metastases_cancer_dead_percentage <- str_c(round(metastases_cancer_dead_percentage, 2), "%")

# Calculate "dead from prostate cancer" percentage for non metastases
nonmetastases_cancer_dead_percentage <- (prostate_data_clean_aug %>% 
                                           filter(status_ == "dead", bone_metastases == "0", dead_from_prostate_cancer == 1) %>% 
                                           nrow())/
  ((prostate_data_clean_aug %>% 
      filter(status_ == "dead", bone_metastases == "0", dead_from_prostate_cancer == 1) %>% 
      nrow())+
     (prostate_data_clean_aug %>% 
        filter(status_ == "dead", bone_metastases == "0", dead_from_prostate_cancer == 0) %>% 
        nrow())) * 100

nonmetastases_cancer_dead_percentage <- str_c(round(nonmetastases_cancer_dead_percentage, 2), "%")

# Plot status vs bone metastases and add the percentage that have died from prostate cancer for each metastases group
status_bm_plot <- prostate_data_clean_aug %>% 
  ggplot(aes(x = bone_metastases, y = status_, color = dead_from_prostate_cancer)) +
  geom_jitter() + 
  annotate("text", x = 1:2, y = 2.55, label = c(nonmetastases_cancer_dead_percentage, metastases_cancer_dead_percentage)) +
  annotate("text", x = 1:2, y = 2.475, label = "(percentage dead from prostate cancer)", size = 3.2) + 
  labs(x = "Bone metastases",
       y = "Status") +
  theme(legend.position = "bottom")


## 3 - Activity percentage stratified by bone metastases

# Group and count and then transform to wide
activity_percentage <- prostate_data_clean_aug %>% 
  group_by(activity, bone_metastases)%>% 
  summarise(counts = n()) %>% 
  pivot_wider(
    names_from = bone_metastases,
    values_from = counts) %>% 
  rename(. ,no_meta = "0", meta = "1")

# Replace NA's with 0
activity_percentage[is.na(activity_percentage)] <- 0  

# Calculate percentage from count and plot 
activity_bm_plot <- activity_percentage %>% 
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "%")


## 4 - Serum hemoglbin vs bone metastases stratified by age group
hemoglobin_bm_boxplot <- prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(y = serum_hemoglobin, x = bone_metastases, color = Age_group)) +
  geom_boxplot()

## 5 - Tumor size vs bone metastases strafified by dead_from_prostate_cancer
size_bm_boxplot <- prostate_data_clean_aug %>% 
  ggplot(aes(y = tumor_size, x = bone_metastases, color = dead_from_prostate_cancer)) +
  geom_boxplot()


## 6 - Bone metastases vs estrogen and status 
# Shows that no patients with the low doses and bone metastases are alive, 
# but at the same time, the higher dose seems to be bad if you have bone metastases

# Group and count for each group, and then transform to wide
# Pivot_wide to have percentage for each bone_metastases group and just the total count
estrogen_metastases_percentage <- prostate_data_clean_aug %>% 
  group_by(estrogen_mg, bone_metastases, status_)%>% 
  summarise(counts = n()) %>% 
  pivot_wider(
    names_from = bone_metastases,
    values_from = counts) %>% 
  rename(. ,no_meta = "0", meta = "1")

# Replace NA's with 0
estrogen_metastases_percentage[is.na(estrogen_metastases_percentage)] <- 0  

# Calculate percentage and plot 
estrogen_bm_status_plot <- estrogen_metastases_percentage %>% 
  mutate(
    no = no_meta/sum(estrogen_metastases_percentage$no_meta) * 100,
    yes = meta/sum(estrogen_metastases_percentage$meta) * 100
  ) %>% 
  pivot_longer(
    -c(estrogen_mg, status_, meta, no_meta),
    names_to = "bone_metastases",
    values_to = "percentage"
  ) %>% 
  ggplot(aes(x = estrogen_mg, y = percentage, fill = bone_metastases)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  facet_wrap(~ status_) +
  labs(y = "%")


## 7 - Age_group vs estrogen and status
# This plot should be seen in relation to plot no. 6

# Group, count and transform to wide 
# row 42 is removed due to age = NA
# Pivot_wide to have percentage for each Age_group group and not as a total count
estrogen_age_percentage <- prostate_data_clean_aug[-42,] %>% 
  group_by(estrogen_mg, status_, Age_group)%>% 
  summarise(counts = n()) %>% 
  pivot_wider(
    names_from = Age_group,
    values_from = counts) %>% 
  rename(. ,fortyfive = "45 - 59", sixty = "60 - 69", seventy = "70 - 79", eighty = "80 - 90")

# Replace NA's with 0
estrogen_age_percentage[is.na(estrogen_age_percentage)] <- 0  

# Calculate percentage and plot 
estrogen_age_status_plot <- estrogen_age_percentage %>% 
  mutate( 
    "45 - 59" = fortyfive/sum(estrogen_age_percentage$fortyfive) * 100,
    "60 - 69" = sixty/sum(estrogen_age_percentage$sixty) * 100,
    "70 - 79" = seventy/sum(estrogen_age_percentage$seventy) * 100,
    "80 - 90" = eighty/sum(estrogen_age_percentage$eighty) * 100
  ) %>% 
  pivot_longer(
    -c(estrogen_mg, status_, fortyfive, sixty, seventy, eighty),
    names_to = "Age_group",
    values_to = "percentage"
  ) %>% 
  ggplot(aes(x = estrogen_mg, y = percentage, fill = Age_group)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  facet_wrap(~ status_) +
  labs(y = "%")


# Export png files
# ------------------------------------------------------------------------------
ggsave(filename = "results/07_PA_death_plot.png",
       plot = PA_death_plot,
       height = 15,
       width = 15,
       units = "cm")

ggsave(filename = "results/07_status_bm_plot.png",
       plot = status_bm_plot,
       height = 15,
       width = 15,
       units = "cm")

ggsave(filename = "results/07_activity_bm_plot.png", 
       plot = activity_bm_plot,
       height = 15,
       width = 15,
       units = "cm")

ggsave(filename = "results/07_hemoglobin_bm_boxplot.png",
       plot = hemoglobin_bm_boxplot,
       height = 15,
       width = 15,
       units = "cm")

ggsave(filename = "results/07_size_bm_boxplot.png",
       plot = size_bm_boxplot,
       height = 15,
       width = 15,
       units = "cm")

ggsave(filename = "results/07_estrogen_bm_status_plot.png",
       plot = estrogen_bm_status_plot,
       height = 15,
       width = 15,
       units = "cm")

ggsave(filename = "results/07_estrogen_age_status_plot.png",
       plot = estrogen_age_status_plot,
       height = 15,
       width = 15,
       units = "cm")

