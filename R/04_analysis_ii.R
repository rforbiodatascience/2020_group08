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

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "Data/02_prostate_data_clean.tsv") %>% 
  as_tibble()

# Scatter plots
# ------------------------------------------------------------------------------


# Wrangle data
# ------------------------------------------------------------------------------

# Wrangle to long format omitting systolic_bp, since this variable is investigated
# All character columns are also omitted as these are used for stratification
pdc_long_systolic_bp <- prostate_data %>% 
  pivot_longer(-c(patno, systolic_bp, activity, status_, cause_of_death, ekg, sdate),
               names_to = "variable",
               values_to = "value")

pdc_long_systolic_bp %>% 
  ggplot(aes(y = systolic_bp, x = value, color = activity)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_systolic_bp %>% 
  ggplot(aes(y = systolic_bp, x = value, color = ekg)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_systolic_bp %>% 
  ggplot(aes(y = systolic_bp, x = value, color = status_)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_systolic_bp %>% 
  ggplot(aes(y = systolic_bp, x = value, color = cause_of_death)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)


# Wrangle to long format omitting systolic_bp, since this variable is investigated
# All character columns are also omitted as these are used for stratification
pdc_long_diastolic_bp <- prostate_data %>% 
  pivot_longer(-c(patno, diastolic_bp, activity, status_, cause_of_death, ekg, sdate),
               names_to = "variable",
               values_to = "value")

pdc_long_diastolic_bp %>% 
  ggplot(aes(y = diastolic_bp, x = value, color = activity)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_diastolic_bp %>% 
  ggplot(aes(y = diastolic_bp, x = value, color = ekg)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_diastolic_bp %>% 
  ggplot(aes(y = diastolic_bp, x = value, color = status_)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)

pdc_long_diastolic_bp %>% 
  ggplot(aes(y = diastolic_bp, x = value, color = cause_of_death)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 3)


# ------------------------------------------------------------------------------
# PCA analysis
# For the analysis we are going to explore groupings in the data based on different variables

# Replacing missing values (NA) in the cause_of_death column with "none"
prostate_data$cause_of_death <- replace_na(prostate_data$cause_of_death, "none")

# Dropping missing rows from the prostate_data
prostate_data <- prostate_data%>% 
  drop_na()

# Selecting only the numerical variables for the analysis
pca_prostate_data <- prostate_data %>% 
  as_tibble %>% 
  select(-patno, -activity, -ekg, -status_, -cause_of_death) %>% 
  drop_na

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
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = cause_of_death)) +
  geom_point()
         
# Visualizing the PCA based on status_
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = status_)) +
  geom_point()

# Visualizing the PCA based on ekg
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = ekg)) +
  geom_point()

# Visualizing the PCA based on cause of death
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = activity)) +
  geom_point()

# Visualizing the PCA based on status and history of CD
prostate_pca_aug %>%
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = activity, shape = status_)) +
  geom_point()

