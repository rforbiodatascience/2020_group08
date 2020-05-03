# Required packdbps
library(tidyverse)
library(modelr)
options(na.action = na.warn)

# Load the data
prostate_data <- read_xlsx(file = "/cloud/project/prostate_data.xlsx")
Pdata <- prostate_data
Pdata

# Plot data set x=sg , y=dbp
ggplot(Pdata, aes(x=sg,y=dbp)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

# Put models on the plot
# 250 models, not all of them fit the data well!
ggplot(Pdata, aes(x=sg,y=dbp)) +
  geom_abline(aes(intercept=a1, slope=a2), data=models, alpha=0.25) +
  geom_point()

# Make the model an R function
# Takes model parameters as input
# Outputs model predictions
model1 <- function(a, data) {
  a[1] + data$sg * a[2]
}
model1(c(7, 1.5), Pdata)

# Calculate RMSD
measure_distance <- function(mod, data) {
  diff <- data$dbp - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), Pdata)

# Use purrr to compute RMSD for all models on the plot
Pdata_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), Pdata)
}

models <- models %>% 
  mutate(dist = purrr::mdbp2_dbl(a1, a2, Pdata_dist))
models

# Put the 10 best models on the data (lowest RMSD)
ggplot(Pdata, aes(x=sg, y=dbp)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

# The 10 best models highlighted in red
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

# A grid search for the 10 best models
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::mdbp2_dbl(a1, a2, Pdata_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

# Put these models back on the original data
ggplot(Pdata, aes(x=sg, y=dbp)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

# Find the best model using optim()
best <- optim(c(0, 0), measure_distance, data = Pdata)
best$par

ggplot(Pdata, aes(x=sg, y=dbp)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])
