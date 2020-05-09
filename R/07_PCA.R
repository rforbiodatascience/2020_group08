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
library(broom)

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "Data/02_prostate_data_clean.tsv") %>% 
  as_tibble()

# ------------------------------------------------------------------------------
# Wrangle data

# Replacing missing values (NA) in the cause_of_death column with "none"
prostate_data$cause_of_death <- replace_na(prostate_data$cause_of_death, "none")

# Dropping missing rows from the prostate_data
prostate_data <- prostate_data %>% 
  select(-study_date) %>% 
  drop_na()

# We now need to normalize our data
# Creating function for normalization
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

# Normalizing all columns (variables)
prostate_data$stage <- normalize(prostate_data$stage)
prostate_data$months_of_follow_up <- normalize(prostate_data$months_of_follow_up)
prostate_data$age <- normalize(prostate_data$age)
prostate_data$weight_index <- normalize(prostate_data$weight_index)
prostate_data$history_of_CD <- normalize(prostate_data$history_of_CD)
prostate_data$systolic_bp <- normalize(prostate_data$systolic_bp)
prostate_data$diastolic_bp <- normalize(prostate_data$diastolic_bp)
prostate_data$serum_hemoglobin <- normalize(prostate_data$serum_hemoglobin)
prostate_data$tumor_size <- normalize(prostate_data$tumor_size)
prostate_data$stage_grade_index <- normalize(prostate_data$stage_grade_index)
prostate_data$PA_phosphatase <- normalize(prostate_data$PA_phosphatase)
prostate_data$bone_metastases <- normalize(prostate_data$bone_metastases)
prostate_data$bone_metastases <- normalize(prostate_data$bone_metastases)
prostate_data$estrogen_mg <- normalize(prostate_data$estrogen_mg)

# Selecting only the numerical variables for the analysis
pca_prostate_data <- prostate_data %>% 
  as_tibble %>% 
  select(-patno, -activity, -ekg, -status_, -cause_of_death, -status) %>% 
  drop_na

# ------------------------------------------------------------------------------
# PCA analysis
# We are going to explore groupings in the data based on different variables

# Compute principal components
prostate_pca <- pca_prostate_data %>% 
  prcomp(center = TRUE, scale. = TRUE)

# Using broom to tidy data
prostate_pca %>% tidy("pcs")

# Plotting the percentage of the variance explained for each principal component
prostate_pca %>% tidy("pcs") %>% 
  ggplot(mapping = aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

# Using broom to tidy
prostate_pca %>% tidy("samples")

# Using broom to augment
prostate_pca_aug <- prostate_pca %>% augment(prostate_data)

# Visualizing the PCA based on cause of death
p1 <- prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = cause_of_death)) +
  geom_point()

# Visualizing the PCA based on status_
p2 <- prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = status_)) +
  geom_point()

# Visualizing the PCA based on ekg
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = ekg)) +
  geom_point()

# Visualizing the PCA based on activity
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = activity)) +
  geom_point()

# Visualizing the PCA based on status and activity
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = activity, shape = status_)) +
  geom_point()

# Visualizing the PCA based on bone-metastases
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = bone_metastases)) +
  geom_point()