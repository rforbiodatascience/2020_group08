install.packages("xlsx")

library(xlsx)

prostate <- read.xlsx("_raw/prostate.xlsx", sheetIndex = 1)
