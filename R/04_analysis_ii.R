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


# Boxplots
# ------------------------------------------------------------------------------


# Scatter plots
# ------------------------------------------------------------------------------

# Scatter plots for systolic_bp correlations
ggplot(data = prostate_data, mapping = aes(x = months_of_follow_up, y = systolic_bp)) +
  geom_point()

ggplot(data = prostate_data, mapping = aes(x = age, y = systolic_bp, color = history_of_CD)) +
  geom_point()

ggplot(data = prostate_data, mapping = aes(x = weight_index, y = systolic_bp)) +
  geom_point()

ggplot(data = prostate_data, mapping = aes(x = diastolic_bp, y = systolic_bp, color = ekg)) +
  geom_point()

ggplot(data = prostate_data, mapping = aes(x = serum_hemoglobin, y = systolic_bp)) +
  geom_point()

ggplot(data = prostate_data, mapping = aes(x = tumor_size, y = systolic_bp)) +
  geom_point()

ggplot(data = prostate_data, mapping = aes(x = stage_grade_index, y = systolic_bp)) +
  geom_point()

ggplot(data = prostate_data, mapping = aes(x = PA_phosphatase, y = systolic_bp)) +
  geom_point()

ggplot(data = prostate_data, mapping = aes(x = estrogen_mg, y = systolic_bp)) +
  geom_point()
