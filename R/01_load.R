# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Install packages and load libraries
# ------------------------------------------------------------------------------
install.packages("xlsx")
library(xlsx)

install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
# Load data
# ------------------------------------------------------------------------------
prostate_data <- read.xlsx("Data/_raw/prostate.xlsx", sheetIndex = 1)

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_data,
          path = "Data/01_prostate_data.tsv")
