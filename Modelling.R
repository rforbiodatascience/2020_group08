# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
install.packages(tidyverse)
library(tidyverse)
library(tibble)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(modelr)

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "Data/02_prostate_data_clean.tsv") %>% 
  as_tibble()


# Some informative plots to watch for linear trends
# Plotting prostate data age vs weight_index grouped by cause_of_death
prostate_data %>% 
  ggplot(mapping = aes(x = age,y = weight_index, group = cause_of_death, color = cause_of_death)) +
  geom_line(alpha = 1/3)

# Making a model for prostate cancer cause of deaths (using months_of_follow_up and tumor_size as variables)
prostate_cancer <- filter(prostate_data, cause_of_death == "prostate cancer")
prostate_cancer %>% 
  ggplot(mapping = aes(x = months_of_follow_up, y = tumor_size)) + 
  geom_line() + 
  ggtitle("Full data = ")

# Creating the linear model and plotting predictions
prostate_cancer_mod <- lm(tumor_size ~ months_of_follow_up, data = prostate_cancer)
prostate_cancer %>% 
add_predictions(prostate_cancer_mod) %>%
  ggplot(mapping = aes(x = months_of_follow_up, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

# Plotting the residuals of the model
prostate_cancer %>% 
  add_residuals(prostate_cancer_mod) %>% 
  ggplot(mapping = aes(x = months_of_follow_up, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")


# We now group the prostate_data by cause_of_death and then nest it
by_cause_of_death <- prostate_data %>% 
  group_by(cause_of_death) %>% 
  nest()

# We now create a function to fit models the different cause of deaths
cause_of_death_model <- function(df) {
  lm(tumor_size ~ months_of_follow_up, data = df)
}

# Apply the function to every data frame
models <- map(by_cause_of_death$data, cause_of_death_model)

# The models are now stored in a column in the by_cause_of_death dataframe
by_cause_of_death <- by_cause_of_death %>% 
  mutate(model = map(data, cause_of_death_model))

# Adding the residuals to the models
by_cause_of_death <- by_cause_of_death %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )    


# To make plotting easier, we now unnest the residuals of the df
resids <- unnest(by_cause_of_death, resids)

# Plotting the residuals of the models
resids %>% 
  ggplot(mapping = aes(x = months_of_follow_up, resid)) +
  geom_line(aes(group = cause_of_death), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

# Facetting by cause_of_death
resids %>% 
  ggplot(aes(months_of_follow_up, resid, group = cause_of_death)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~cause_of_death)

# We can now use broom::glance to get some quality measures of the models
glance <- by_cause_of_death %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

# Looking for models that aren't fitting well
glance %>% 
  arrange(r.squared)

# Plotting r-squared for each cause_of_death
glance %>% 
  ggplot(aes(cause_of_death, r.squared)) + 
  geom_jitter(width = 0.5)

# The models all seem to fit pretty badly