# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())


# load libraries
# ------------------------------------------------------------------------------

library(xlsx)
library(tidyverse)


# Load data
# ------------------------------------------------------------------------------
age <- read.xlsx("Data/_raw/age.xlsx", sheetIndex = 1)
ap <- read.xlsx("Data/_raw/ap.xlsx", sheetIndex = 1)
bm <- read.xlsx("Data/_raw/bm.xlsx", sheetIndex = 1)
dbp <- read.xlsx("Data/_raw/dbp.xlsx", sheetIndex = 1)
dtime <- read.xlsx("Data/_raw/dtime.xlsx", sheetIndex = 1)
ekg <- read.xlsx("Data/_raw/ekg.xlsx", sheetIndex = 1)
hg <- read.xlsx("Data/_raw/hg.xlsx", sheetIndex = 1)
hx <- read.xlsx("Data/_raw/hx.xlsx", sheetIndex = 1)
pf <- read.xlsx("Data/_raw/pf.xlsx", sheetIndex = 1)
rx <- read.xlsx("Data/_raw/rx.xlsx", sheetIndex = 1)
sbp <- read.xlsx("Data/_raw/sbp.xlsx", sheetIndex = 1)
sdate <- read.xlsx("Data/_raw/sdate.xlsx", sheetIndex = 1)
sg <- read.xlsx("Data/_raw/sg.xlsx", sheetIndex = 1)
stage <- read.xlsx("Data/_raw/stage.xlsx", sheetIndex = 1)
status <- read.xlsx("Data/_raw/status.xlsx", sheetIndex = 1)
sz <- read.xlsx("Data/_raw/sz.xlsx", sheetIndex = 1)
wt <- read.xlsx("Data/_raw/wt.xlsx", sheetIndex = 1)


# Wrangle data
# ------------------------------------------------------------------------------

# Join data by patient number
prostate_data_joined <- left_join(age, ap, by = "patno") %>% 
  left_join(., bm, by = "patno") %>%
  left_join(., dbp, by = "patno") %>% 
  left_join(., dtime, by = "patno") %>% 
  left_join(., ekg, by = "patno") %>% 
  left_join(., hg, by = "patno") %>% 
  left_join(., hx, by = "patno") %>% 
  left_join(., pf, by = "patno") %>% 
  left_join(., rx, by = "patno") %>% 
  left_join(., sbp, by = "patno") %>% 
  left_join(., sdate, by = "patno") %>% 
  left_join(., sg, by = "patno") %>%
  left_join(., stage, by = "patno") %>% 
  left_join(., status, by = "patno") %>% 
  left_join(., sz, by = "patno") %>% 
  left_join(., wt, by = "patno")


# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_data_joined,
          path = "Data/01_prostate_data.tsv")
