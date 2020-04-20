# Clear workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load the data
prostate_data_clean <- read_tsv(file = "PROJECT FILES/Data/02_prostate_data_clean.tsv")
data <- prostate_data_clean

### serum_hemoglobin ###

# Plot serum_hemoglobin vs status_
SG <- ggplot(data=prostate_data_clean, aes(x=serum_hemoglobin, fill=status_)) +
  geom_density(alpha = 0.5, adjust = 0.5, kernel="gaussian") 
SG

# Plot serum_hemoglobin vs activity
SG1 <- ggplot(data=prostate_data_clean, 
       mapping=aes(x=activity, y=serum_hemoglobin, fill = factor(activity))) + 
  geom_violin(alpha = 0.5) +
  geom_boxplot(alpha = 0.6)

SG2 <- ggplot(data=prostate_data_clean, aes(x=serum_hemoglobin, fill=activity)) +
  geom_density(alpha = 0.5, adjust = 0.5, kernel="gaussian") 

grid.arrange(G1, G2, nrow = 2)

### EKG ###

# Plot ekg vs history_of_CD
E1 <- ggplot(data=prostate_data_clean, aes(x=history_of_CD, y=ekg)) + 
  geom_bar(stat = "identity") 

# Plot ekg vs PA_phosphatase
E2 <- ggplot(data=prostate_data_clean, aes(x=PA_phosphatase, y=ekg)) + 
  geom_bar(stat = "identity") 

# Plot ekg vs activity
E3 <- ggplot(data=prostate_data_clean, aes(x=ekg, colour=activity)) +
  geom_density(alpha = 0.4, adjust = 0.1, kernel="gaussian") 

grid.arrange(E1, E2, E3, nrow = 3)

### Tumor size ###


# Plot tumor_size vs stage_grade_index
SZ1 <- ggplot(data=prostate_data_clean, 
       mapping=aes(x=stage_grade_index, y=tumor_size, fill = factor(stage_grade_index))) + 
  geom_boxplot(alpha = 0.6)


# Plot tumor_size vs status_
SZ2 <- ggplot(data=prostate_data_clean, aes(x=tumor_size, fill=status_)) +
  geom_density(alpha = 0.5, adjust = 0.5, kernel="gaussian") 


# Plot tumor_size vs activity
SZ3 <- ggplot(data=prostate_data_clean, aes(x=tumor_size, fill=activity)) +
  geom_density(alpha = 0.5, adjust = 0.5, kernel="gaussian") 


# Plot tumor_size vs bone_metastases
SZ4 <- ggplot(data=prostate_data_clean, 
       mapping=aes(x=bone_metastases, y=tumor_size, fill = factor(bone_metastases))) + 
  geom_violin(alpha = 0.5) +
  geom_boxplot(alpha = 0.6)

grid.arrange(SZ1, SZ4, ncol = 2)
grid.arrange(SZ2, SZ3, nrow = 2)


### Stage grade index ###

# Plot stage_grade_index vs status_
SGI <- ggplot(data=prostate_data_clean, aes(x=stage_grade_index, fill=status_)) +
  geom_density(alpha = 0.5, adjust = 0.5, kernel="gaussian") 

# Plot stage_grade_index vs activity
SGI2 <- ggplot(data=prostate_data_clean, aes(x=stage_grade_index, fill=activity)) +
  geom_density(alpha = 0.5, adjust = 0.5, kernel="gaussian") 

grid.arrange(SGI, SGI2, nrow = 2)

