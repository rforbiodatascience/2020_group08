# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(gridExtra)

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

# Model of diastolic vs systolic bp grouped by cause_of_death

# Define linear regression
bp_model <- function(prostate_data_clean_aug) {
  lm(systolic_bp ~ diastolic_bp, data = prostate_data_clean_aug)
}

# Remove patients that are alive with na.omit(), since "alive" is NA in cause_of_death column
# and nest
cause_of_death_nest <- prostate_data_clean_aug %>% 
  na.omit() %>% 
  group_by(cause_of_death) %>% 
  nest() %>% 
  mutate(model = map(data, bp_model))

bp_cause_glance <- cause_of_death_nest %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance)

# Extract adjusted r^2 for p-values below 0.05 to plot model fit
lm_bp_model_fit <- bp_cause_glance %>% 
  filter(adj.r.squared > 0.05) %>% 
  select(adj.r.squared, p.value, cause_of_death) %>% 
  arrange(desc(adj.r.squared)) %>% 
  ggplot(aes(x = fct_reorder(cause_of_death, adj.r.squared, .fun = max, .desc = T), y = adj.r.squared)) +
  geom_col(width = 0.6) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(size=12)) +
  labs(x = "Cause of death",
       y = "Adjusted R^2",
       title = "Fit of systolic vs diastolic blood pressure linear regression model") +
  ylim(0, 1)

# Plot of systolic via distolic bp by cause of death to compare with the R^2 values
sdp_vs_dbp_plot <- prostate_data_clean_aug %>% 
  na.omit() %>% 
  ggplot(aes(x = diastolic_bp, y = systolic_bp)) +
  geom_jitter() + 
  geom_smooth(method = lm, se = T ) + 
  facet_wrap(~ cause_of_death) +
  labs(x = "diastolic blood pressure/10",
       y = "systolic blood pressure/10")

lm_bp_plots <- grid.arrange(sdp_vs_dbp_plot, lm_bp_model_fit, ncol = 2)

# Export png files
# ------------------------------------------------------------------------------
ggsave(filename = "results/05_lm_bp_plots.png",
       plot = lm_bp_plots,
       height = 12,
       width = 32,
       units = "cm")


