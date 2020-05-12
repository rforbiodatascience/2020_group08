# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(forcats)
library(caret)
library(caretEnsemble)

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

# Predicting if a patient died from prostate cancer or not with logistic regression

# Remove all the rows where the patients are still alive and rows containing NA's as 
# the model cant deal with these, as well as irrelevant columns

prostate_model_data <- prostate_data_clean_aug %>% 
  filter(status_ == "dead") %>% 
  select(-c(patno, study_date, status_, cause_of_death, Age_group)) %>% 
  na.omit()

# Make logistic regression model 
prostate_death_model <- glm(dead_from_prostate_cancer~.,
                            family = binomial(link = "logit"),
                            data = prostate_model_data)

# Extract variables from the model with p < 0.05
summary(prostate_death_model)$coef[summary(prostate_death_model)$coef[,4] <= .05,] 

# Put the significant varaibles into new revised model 
# (the model with all variables will cause an error in the 5-fold cross validation)
prostate_death_model_revised <- glm(dead_from_prostate_cancer ~ age + PA_phosphatase +
                                      diastolic_bp + months_of_follow_up + stage_grade_index +
                                      tumor_size,
                                    family = binomial(link = "logit"),
                                    data = prostate_model_data)

## Running 5-fold cross validation 

# Define training control (set.seed for reproducibility)
set.seed(123)

train.control <- trainControl(method = "cv", number = 5)

# Train the model
prostate_death_model_5fold <- train(dead_from_prostate_cancer ~ months_of_follow_up + age + 
                                      diastolic_bp + tumor_size + stage_grade_index + 
                                      PA_phosphatase,
                                    data = prostate_model_data, 
                                    method = "glm",
                                    trControl = train.control, family = binomial(link = "logit"))

print(prostate_death_model_5fold)

# extract accuracy and save as string
prostate_death_model_5fold_accuracy <- getMetric(prostate_death_model_5fold) * 100

prostate_death_model_5fold_accuracy <- str_c("model accuracy = ",
                                             (round(prostate_death_model_5fold_accuracy, 2)), "%")

# Run anova to quntify how much the individual variables explain of the variance
prostate_death_model_anova <- anova(prostate_death_model_revised, test = "Chisq")

anova(prostate_death_model_revised, test = "Chisq")

glm_death_model_performance <- prostate_death_model_anova %>% 
  as.data.frame() %>% 
  drop_na() %>% 
  rownames_to_column("variable") %>% 
  ggplot(aes(x = fct_reorder(variable, Deviance, .fun = max, .desc = T), y = Deviance)) +
  geom_col(width = 0.6) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(size=10)) +
  labs(x = "Variable",
       y = "Deviance (from ANOVA)",
       title = "Prediction of cause of death (prostate cancer vs other) with logistic regression") +
  annotate("text", x = 4.5, y = 32.5, label = prostate_death_model_5fold_accuracy)

# Export png files
# ------------------------------------------------------------------------------
ggsave(filename = "results/04_glm_death_model_performance.png",
       plot = glm_death_model_performance,
       height = 10,
       width = 14,
       units = "cm")
