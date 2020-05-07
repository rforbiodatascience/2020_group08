# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(keras)
library(tensorflow)
library(tidyverse)

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "Data/02_prostate_data_clean.tsv") %>% 
  as_tibble()

# ------------------------------------------------------------------------------
# Building an ANN model to predict cause_of_death

# Replacing missing values (NA) in the cause_of_death column with "none"
prostate_data$cause_of_death <- replace_na(prostate_data$cause_of_death, "none")

# Dropping missing rows from the prostate_data and select only the 4 variables we use for the model
prostate_data <- prostate_data %>% 
  select(-patno, -activity, -ekg, -status_, -study_date, -status) %>% 
  drop_na()

# Converting character labels for cause of death to numeric
nn_dat = prostate_data %>%
  rename(PA_phosphatase_feat = PA_phosphatase,
         tumor_size_feat = tumor_size,
         estrogen_mg_feat = estrogen_mg,
         serum_hemoglobin_feat = serum_hemoglobin,
         age_feat = age,
         bone_metastases_feat = bone_metastases,
         diastolic_bp_feat = diastolic_bp,
         systolic_bp_feat = systolic_bp,
         months_of_follow_up_feat = months_of_follow_up,
         history_of_CD_feat = history_of_CD,
         stage_grade_index_feat = stage_grade_index,
         stage_feat = stage,
         weight_index_feat = weight_index) %>%
  mutate(class_num = case_when(cause_of_death == "none" ~ 0,
                               cause_of_death == "cerebrovascular disease/accident" ~ 1,
                               cause_of_death == "heart or vascular disease" ~ 2,
                               cause_of_death == "other cancer" ~ 3,
                               cause_of_death == "other specified non-cancer" ~ 4,
                               cause_of_death == "prostate cancer" ~ 5,
                               cause_of_death == "pulmonary embolus" ~ 6,
                               cause_of_death == "respiratory disease" ~ 7,
                               cause_of_death == "unknown" ~ 8,
                               cause_of_death == "unspecified non-cancer" ~ 9), # factor, so = 0, 1, 2
         class_label = cause_of_death)
nn_dat %>% head(3)

# Splitting the data into a training and a test data set, setting aside 20 % of the data for left out data partition, to be used for final performance evaluation
test_f = 0.20
nn_dat = nn_dat %>%
  mutate(partition = sample(x = c('train','test'),
                            size = nrow(.),
                            replace = TRUE,
                            prob = c(1 - test_f, test_f)))
nn_dat %>% count(partition)

# Based on the partition, we now create a training and test data
x_train = nn_dat %>%
  filter(partition == 'train') %>%
  select(contains("feat")) %>%
  as.matrix
y_train = nn_dat %>%
  filter(partition == 'train') %>%
  pull(class_num) %>%
  to_categorical(10)

x_test = nn_dat %>%
  filter(partition == 'test') %>%
  select(contains("feat")) %>%
  as.matrix
y_test = nn_dat %>%
  filter(partition == 'test') %>%
  pull(class_num) %>%
  to_categorical(10)

# Defining ANN model
model = keras_model_sequential() %>% 
  layer_dense(units = 4, activation = 'relu', input_shape = 13) %>% 
  layer_dense(units = 10, activation = 'softmax')

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
      epochs = 1000,
      batch_size = 20,
      validation_split = 0
  )

# Inspect the training process
plot(history)

# Final performance evaluation
perf = model %>% evaluate(x_test, y_test)
perf