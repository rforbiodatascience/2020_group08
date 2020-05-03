# Required libraries
library(modelr)
library(tidyverse)

# Load the data
prostate_data <- read_xlsx(file = "/cloud/project/prostate_data.xlsx")
Pdata <- prostate_data
Pdata
stage

# What's the relationship between x and y when 
# sorted by a particular group? 

# Plot the 3 variables
Pdata %>% 
  ggplot(aes(x=sbp, y=dbp, group=ekg)) +
  geom_line(alpha = 1/3)

# Nested data frame, gather by stage
# Like tables within tables
by_ekg <- Pdata %>% 
  group_by(ekg, stage) %>% 
  nest()
by_ekg

# All data for 1 specific ekg category
by_ekg$data[[1]]

# Model fitting & apply to every data frame
ekg_model <- function(df) {
  lm(sbp ~ dbp, data = df)
}
models <- map(by_ekg$data, ekg_model)

# Creating a new variable (instead of a new object)
# Mutate is a dplyr variable
# Keeps all related objects together
# even when filtered / arranged
by_ekg <- by_ekg %>% 
  mutate(model = map(data, ekg_model))
by_ekg
by_ekg %>% 
  filter(stage == "3")

# Residuals
by_ekg <- by_ekg %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_ekg

# Convert to normal data frame by unnesting
resids <- unnest(by_ekg, resids)
resids

# Plot residuals
resids %>% 
  ggplot(aes(dbp, resid)) +
  geom_line(aes(group = ekg), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

# By stage:
resids %>% 
  ggplot(aes(dbp, resid, group = ekg)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~stage)

# Model quality: data frame per country
by_ekg %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

# Suppress list columns
glance <- by_ekg %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance

# Look for models that don't fit
# High RMSD
glance %>% 
  arrange(r.squared)

# Check where the worst models are by plotting
glance %>% 
  ggplot(aes(stage, r.squared)) + 
  geom_jitter(width = 0.5)

# Find the worst RMSD countries
bad_fit <- filter(glance, r.squared < 0.25)

Pdata %>% 
  semi_join(bad_fit, by = "ekg") %>% 
  ggplot(aes(dbp, sbp, colour = ekg)) +
  geom_line()




