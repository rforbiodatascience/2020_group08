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
prostate_data <- prostate_data %>% 
  select(-dead_from_prostate_cancer, -Age_group) %>% 
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
prostate_data$sdate <- normalize(prostate_data$sdate)
prostate_data$bone_metastases <- normalize(prostate_data$bone_metastases)
prostate_data$estrogen_mg <- normalize(prostate_data$estrogen_mg)

# Selecting only the numerical variables for the analysis
pca_prostate_data <- prostate_data %>% 
  as_tibble %>% 
  select(-patno, -activity, -ekg, -status_, -cause_of_death,) %>% 
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

# ------------------------------------------------------------------------------
# K-means clustering

prostate_k_org <- prostate_pca_aug %>% 
  select(stage, months_of_follow_up, age, weight_index, history_of_CD, systolic_bp, diastolic_bp, serum_hemoglobin, tumor_size, stage_grade_index, PA_phosphatase, bone_metastases, sdate, estrogen_mg) %>% 
  kmeans(centers = 2)

prostate_pca_aug_k_org <- prostate_k_org %>% 
  augment(prostate_pca_aug) %>% 
  rename(cluster_org = .cluster)

prostate_k_pca <- prostate_pca_aug_k_org %>% 
  select(.fittedPC1, .fittedPC2) %>% 
  kmeans(centers = 2)

prostate_pca_aug_k_org_pca <- prostate_k_pca %>% 
  augment(prostate_pca_aug_k_org) %>% 
  rename(cluster_pca = .cluster)

pl1 <- prostate_pca_aug_k_org_pca %>% 
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = status_)) +
  geom_point() +
  theme(legend.position = "bottom")

pl2 <- prostate_pca_aug_k_org_pca %>% 
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_org)) +
  geom_point() +
  theme(legend.position = "bottom")

pl3 <- prostate_pca_aug_k_org_pca %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(legend.position = "bottom")

pl1
pl2
pl3

# ------------------------------------------------------------------------------
# Building an ANN model to predict cause_of_death

# Loading libraries
library(tidyverse)
library(devtools)
library(keras)

# Replacing missing values (NA) in the cause_of_death column with "none"
prostate_data$cause_of_death <- replace_na(prostate_data$cause_of_death, "none")

# Dropping missing rows from the prostate_data and select only the 4 variables we use for the model
prostate_data <- prostate_data %>% 
  select(-dead_from_prostate_cancer, -Age_group, -patno, -stage, -months_of_follow_up, -age, -weight_index, -activity, -history_of_CD, -systolic_bp, -diastolic_bp, -ekg, -stage_grade_index, -bone_metastases, -sdate, -status_) %>% 
  drop_na()

# Converting character labels for cause of death to numeric
nn_dat = prostate_data %>%
  rename(PA_phosphatase_feat = PA_phosphatase,
         tumor_size_feat = tumor_size,
         estrogen_mg_feat = estrogen_mg,
         serum_hemoglobin_feat = serum_hemoglobin) %>%
  mutate(class_num = case_when(cause_of_death == "none" ~ 1,
                               cause_of_death == "cerebrovascular disease/accident" ~ 2,
                               cause_of_death == "heart or vascular disease" ~ 3,
                               cause_of_death == "other cancer" ~ 4,
                               cause_of_death == "other specified non-cancer" ~ 5,
                               cause_of_death == "prostate cancer" ~ 6,
                               cause_of_death == "pulmonary embolus" ~ 7,
                               cause_of_death == "respiratory disease" ~ 8,
                               cause_of_death == "unknown" ~ 9,
                               cause_of_death == "unspecified non-cancer" ~ 10), # factor, so = 0, 1, 2
         class_label = cause_of_death)
nn_dat %>% head(3)

# Splitting the data into a training and a test data set, setting aside 20% of the data for left out data partition, to be used for final performance evaluation
test_f = 0.20
nn_dat = nn_dat %>%
  mutate(partition = sample(x = c('train','test'),
                            size = nrow(.),
                            replace = TRUE,
                            prob = c(1 - test_f, test_f)))
nn_dat %>% count(partition)

# Based on the partition, we can now create training and test data
x_train = nn_dat %>%
  filter(partition == 'train') %>%
  select(contains("feat")) %>%
  as.matrix
y_train = nn_dat %>%
  filter(partition == 'train') %>%
  pull(class_num) %>%
  to_categorical(3)

x_test = nn_dat %>%
  filter(partition == 'test') %>%
  select(contains("feat")) %>%
  as.matrix
y_test = nn_dat %>%
  filter(partition == 'test') %>%
  pull(class_num) %>%
  to_categorical(3)

# Defining ANN model
model = keras_model_sequential() %>% 
  layer_dense(units = 4, activation = 'relu', input_shape = 4) %>% 
  layer_dense(units = 3, activation = 'softmax')

# Compiling model
model %>%
  compile(loss = 'categorical_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy')
  )

# Summarizing model
model %>%
  summary

# Fit the model and save the training progres in the history object
history = model %>%
  fit(x = x_train,
      y = y_train,
      epochs = 200,
      batch_size = 20,
      validation_split = 0
  )

# Inspect the training process
plot(history)

# Final performance evaluation
perf = model %>% evaluate(x_test, y_test)
perf

# Augmenting nn_dat for plotting
plot_dat = nn_dat %>%
  filter(partition == 'test') %>%
  mutate(class_num = factor(class_num),
         y_pred = factor(predict_classes(model, x_test)),
         Correct = factor(ifelse(class_num == y_pred, "Yes", "No")))
plot_dat %>% select(-contains("feat")) %>% head(3)

# Visualizing confusion matrix
title     = "Classification Performance of Artificial Neural Network"
sub_title = str_c("Accuracy = ", round(perf$acc, 3) * 100, "%")
x_lab     = "True iris class"
y_lab     = "Predicted iris class"
plot_dat %>% ggplot(aes(x = class_num, y = y_pred, colour = Correct)) +
  geom_jitter() +
  scale_x_discrete(labels = levels(nn_dat$class_label)) +
  scale_y_discrete(labels = levels(nn_dat$class_label)) +
  theme_bw() +
  labs(title = title, subtitle = sub_title, x = x_lab, y = y_lab)
