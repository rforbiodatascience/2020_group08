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
library(gridExtra)

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "Data/03_prostate_data_clean_aug.tsv")


# ------------------------------------------------------------------------------
# Wrangle data

# Replacing missing values (NA) in the cause_of_death and dead_from_prostate_cancer column with "none"
prostate_data$cause_of_death <- replace_na(prostate_data$cause_of_death, "none")
prostate_data$dead_from_prostate_cancer <- replace_na(prostate_data$dead_from_prostate_cancer, "none")

#Ensure that factorial variables are actually factors
factor_columns <- c("stage", "activity", "history_of_CD", "ekg", "bone_metastases",
                    "estrogen_mg", "status_", "cause_of_death", "dead_from_prostate_cancer",
                    "Age_group")

prostate_data[factor_columns] <- lapply(prostate_data[factor_columns], factor)

# Dropping missing rows from the prostate_data
prostate_data <- prostate_data %>% 
  select(-study_date) %>% 
  drop_na()

# We now need to normalize our data
# Creating function for normalization
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

# Normalizing all numerical columns (variables)
prostate_data <- prostate_data %>% 
  mutate(months_of_follow_up = normalize(months_of_follow_up),
         age = normalize(age),
         weight_index = normalize(weight_index),
         systolic_bp = normalize(systolic_bp),
         diastolic_bp = normalize(diastolic_bp),
         serum_hemoglobin = normalize(serum_hemoglobin),
         tumor_size = normalize(tumor_size),
         stage_grade_index = normalize(stage_grade_index),
         PA_phosphatase = normalize(PA_phosphatase))

# Selecting only the numerical variables for the analysis
pca_prostate_data <- prostate_data %>% 
  as_tibble %>% 
  select(months_of_follow_up, age, weight_index, systolic_bp, diastolic_bp, serum_hemoglobin,
         tumor_size, stage_grade_index, PA_phosphatase) %>% 
  drop_na()

# ------------------------------------------------------------------------------
# PCA analysis
# We are going to explore groupings in the data based on different variables

# Compute principal components
prostate_pca <- pca_prostate_data %>% 
  prcomp(center = TRUE, scale. = TRUE)

# Using broom to tidy data
prostate_pca %>% tidy("pcs")

# Plotting the percentage of the variance explained for each principal component
PCs_plot <- prostate_pca %>% tidy("pcs") %>% 
  ggplot(mapping = aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw() + 
  labs(y = "% variance explained",
       x = "Principal component #") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

# Using broom to tidy
prostate_pca %>% tidy("samples")

# Using broom to augment
prostate_pca_aug <- prostate_pca %>% augment(prostate_data)

# Visualizing the PCA based on cause of death
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = cause_of_death)) +
  geom_point() +
  labs(x = "PC1", y = "PC2" , colour = "cause of death")

# Visualizing the PCA based on ekg
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = ekg)) +
  geom_point() +
  labs(x = "PC1", y = "PC2", colour = "ekg")

# Visualizing the PCA based on activity
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = activity)) +
  geom_point() +
  labs(x = "PC1", y = "PC2", "activity")

# Visualizing the PCA based on status and activity
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = activity, shape = status_)) +
  geom_point() +
  labs(x = "PC1", y = "PC2")

# Visualizing the PCA based on status_
status_pca_plot <- prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = status_)) +
  geom_point(alpha = 0.5) +
  labs(x = "PC1", y = "PC2", colour = "status") +
  theme(legend.position = "bottom")

# Visualizing the PCA based on bone-metastases
bone_pca_plot <- prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = bone_metastases)) +
  geom_point(alpha = 0.5) +
  labs(x = "PC1", y = "PC2", colour = "bone metastases") +
  theme(legend.position = "bottom")

PCA_plots <- grid.arrange(status_pca_plot, bone_pca_plot, ncol = 2)

# Export png files of relevant plots
# ------------------------------------------------------------------------------
ggsave(filename = "results/07_PC_variance_plot.png",
       plot = PCs_plot,
       height = 9,
       width = 15,
       units = "cm")

ggsave(filename = "results/07_PCA_plots.png",
       plot = PCA_plots,
       height = 9,
       width = 20,
       units = "cm")
