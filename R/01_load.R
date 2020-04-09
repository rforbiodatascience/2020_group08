# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# load libraries
# ------------------------------------------------------------------------------

library(xlsx)
library(tidyverse)


# Load data
# ------------------------------------------------------------------------------
prostate_data <- read.xlsx("Data/_raw/prostate.xlsx", sheetIndex = 1)

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_data,
          path = "Data/01_prostate_data.tsv")
