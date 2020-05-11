# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(keras)
library(tensorflow)

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "Data/03_prostate_data_clean_aug.tsv") %>% 
  as_tibble()

# ------------------------------------------------------------------------------
# Building an ANN model to predict cause_of_death

# Replacing missing values (NA) in the cause_of_death and dead_from_prostate_cancer column with "none"
prostate_data$cause_of_death <- replace_na(prostate_data$cause_of_death, "none")

# Dropping missing rows from the prostate_data and select only the 4 variables we use for the model
prostate_data <- prostate_data %>% 
  select(-patno, -status_, -study_date, -dead_from_prostate_cancer, -Age_group) %>% 
  drop_na()

# Creating numerical classes for the categorical character variables; activity and ekg
prostate_data <- prostate_data %>% 
  mutate(activity = case_when(activity == "normal activity" ~ 0,
                              activity == "in bed < 50% daytime" ~ 1,
                              activity == "in bed > 50% daytime" ~ 2,
                              activity == "confined to bed" ~ 3),
         ekg = case_when(ekg == "benign" ~ 0,
                         ekg == "heart block or conduction def" ~ 1,
                         ekg == "heart strain" ~ 2,
                         ekg == "normal" ~ 3,
                         ekg == "old MI" ~ 4,
                         ekg == "recent MI" ~ 5,
                         ekg == "rhythmic disturb & electrolyte ch" ~ 6))

# We now do one-hot encoding for the categorical variables in the dataset
bone_metastases_cat <- prostate_data %>% 
  pull(bone_metastases) %>% 
  to_categorical %>% 
  as_tibble() %>% 
  rename(bone_metastases_feat_1 = V1,
         bone_metastases_feat_2 = V2)

history_of_CD_cat <- prostate_data %>% 
  pull(history_of_CD) %>% 
  to_categorical %>% 
  as_tibble() %>% 
  rename(history_of_CD_feat_1 = V1,
         history_of_CD_feat_2 = V2)

stage_cat <- prostate_data %>% 
  pull(stage) %>% 
  to_categorical %>% 
  as_tibble() %>% 
  rename(stage_feat_1 = V1,
         stage_feat_2 = V2,
         stage_feat_3 = V3,
         stage_feat_4 = V4,
         stage_feat_5 = V5)

estrogen_mg_cat <- prostate_data %>% 
  pull(estrogen_mg) %>% 
  to_categorical %>% 
  as_tibble() %>% 
  rename(estrogen_mg_feat_1 = V1,
         estrogen_mg_feat_2 = V2,
         estrogen_mg_feat_3 = V3,
         estrogen_mg_feat_4 = V4,
         estrogen_mg_feat_5 = V5,
         estrogen_mg_feat_6 = V6)

activity_cat <- prostate_data %>% 
  pull(activity) %>% 
  to_categorical %>% 
  as_tibble() %>% 
  rename(activity_feat_1 = V1,
         activity_feat_2 = V2,
         activity_feat_3 = V3,
         activity_feat_4 = V4)

ekg_cat <- prostate_data %>% 
  pull(ekg) %>% 
  to_categorical %>% 
  as_tibble() %>% 
  rename(ekg_feat_1 = V1,
         ekg_feat_2 = V2,
         ekg_feat_3 = V3,
         ekg_feat_4 = V4,
         ekg_feat_5 = V5,
         ekg_feat_6 = V6,
         ekg_feat_7 = V7)

# Converting character labels for cause of death to numeric
nn_dat = prostate_data %>%
  rename(PA_phosphatase_feat = PA_phosphatase,
         tumor_size_feat = tumor_size,
         estrogen_mg_feat = estrogen_mg,
         serum_hemoglobin_feat = serum_hemoglobin,
         age_feat = age,
         diastolic_bp_feat = diastolic_bp,
         systolic_bp_feat = systolic_bp,
         months_of_follow_up_feat = months_of_follow_up,
         stage_grade_index_feat = stage_grade_index,
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

# Adding the one-hot encoded variables to nn_dat
nn_dat <- nn_dat %>% 
  add_column(activity_cat) %>% 
  add_column(bone_metastases_cat) %>% 
  add_column(ekg_cat) %>% 
  add_column(estrogen_mg_cat) %>% 
  add_column(history_of_CD_cat) %>% 
  add_column(stage_cat)

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
  layer_dense(units = 6, activation = 'relu', input_shape = 36) %>% 
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
      epochs = 200,
      batch_size = 20,
      validation_split = 0
  )

# Inspect the training process
ann_model_performance <- plot(history)

# Final performance evaluation
perf = model %>% evaluate(x_test, y_test)
perf

# Export png files
# ------------------------------------------------------------------------------
ggsave(filename = "results/08_ANN_model_performance.png",
       plot = ann_model_performance,
       height = 10,
       width = 14,
       units = "cm")
