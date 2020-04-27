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


### Bone metastases + activity

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


### Serum hemoglbin + bone metastases
prostate_data_clean %>% 
  na.omit() %>% 
  ggplot(aes(y = serum_hemoglobin, x = bone_metastases, color = Age_group)) +
  geom_boxplot()

### Tumor size + bone metastases
prostate_data_clean %>% 
  ggplot(aes(y = tumor_size, x = bone_metastases, color = dead_from_prostate_cancer)) +
  geom_boxplot()


###Bone metastases vs estrogen and status 
# Shows that no patients with the low doses and bone metastases are alive, 
# but at the same time, the higher dose seems to be bad if you have bone metastases

# Transform count by grouped by bone_metastases
# Pivot_wide to have percentage for each bone_metastases group and not in total
estrogen_metastases_percentage <- prostate_data_clean %>% 
  group_by(estrogen_mg, bone_metastases, status_)%>% 
  summarise(counts = n()) %>% 
  pivot_wider(
    names_from = bone_metastases,
    values_from = counts) %>% 
  rename(. ,no_meta = "0", meta = "1")

# replace NA's with 0
estrogen_metastases_percentage[is.na(estrogen_metastases_percentage)] <- 0  

# calculate percentage and plot 
estrogen_metastases_percentage %>% 
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
  facet_wrap(~ status_)

### Age_group vs estrogen and status

# Transform count grouped by Age_group (row 42 is removed due to age = NA)
# Pivot_wide to have percentage for each Age_group group and not in total
estrogen_age_percentage <- prostate_data_clean[-42,] %>% 
  group_by(estrogen_mg, status_, Age_group)%>% 
  summarise(counts = n()) %>% 
  pivot_wider(
    names_from = Age_group,
    values_from = counts) %>% 
  rename(. ,fortyfive = "45 - 59", sixty = "60 - 69", seventy = "70 - 79", eighty = "80 - 90")

# replace NA's with 0
estrogen_age_percentage[is.na(estrogen_age_percentage)] <- 0  

# calculate percentage and plot 
estrogen_age_percentage %>% 
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
  facet_wrap(~ status_)


### PA_phosphatase vs stage_grade_index
prostate_data_clean %>% 
  ggplot(aes(y = PA_phosphatase, x = stage_grade_index, color = dead_from_prostate_cancer)) +
  geom_jitter()

### Note: change/look at the plots where na.omit() has been used, since this removes  all rows with a NA in any column ###


##### Predicting if a patient died from prostate cancer or not with logistic regression #####

# Remove all the rows where the patients are still alive and rows containing NA's as the model cant deal with these

prostate_model_data <- prostate_data_clean %>% 
  filter(status_ == "dead") %>% 
  select(-c(patno, sdate, status_, cause_of_death, Age_group)) %>% 
  na.omit()

# Make model and test which parameters are significant
prostate_death_model <- glm(dead_from_prostate_cancer~.,
                            family = binomial(link='logit'),
                            data = prostate_model_data)

# extract variables from the model with p < 0.05
summary(prostate_death_model)$coef[summary(prostate_death_model)$coef[,4] <= .05,] 

# Put the significant varaibles into new revised model 
# (the model with all variables will cause an error in the 5-fold cross validation)
prostate_death_model_revised <- glm(dead_from_prostate_cancer ~ months_of_follow_up + age + 
                                      diastolic_bp + tumor_size + stage_grade_index + PA_phosphatase,
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

prostate_death_model_anova %>% 
  as.data.frame() %>% 
  drop_na() %>% 
  rownames_to_column("variable") %>% 
  ggplot(aes(x = fct_reorder(variable, Deviance, .fun = max, .desc = T), y = Deviance)) +
  geom_col(width = 0.6) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size=12)) +
  labs(x = "Variable",
       y = "Deviance (from ANOVA)",
       title = "Prediction of cause of death (prostate cancer vs other) with logistic regression") +
  annotate("text", x = 4.5, y = 32.5, label = prostate_death_model_5fold_accuracy)


