# Simmulation exercise
# This script simmulates a set of whales to match the tip of
# snout-center of eye ratio graph in Figure 4 on Nishiwaki 1963.

# Load required packages
library(tidyverse)
library(rstan)
set.seed(1991)

# 1. generate ratios (R) for male and females to match Nishiwaki-----
n <- 30 # number of whales of each sex
min.L <- 4 # length at birth
max.L.F <- 11 # max length females
mean.L.F <- 8 # guestimated mean length of females

# simmulate female lengths - truncated normal distribution
x_F <- rnorm(n, mean = mean.L.F, sd = (max.L.F - mean.L.F) / 3) %>%
  # Truncate values to be between min and max length
  pmax(min.L) %>%
  pmin(max.L.F)

# Verify the distribution
hist(x_F, breaks = 10, main = "Distribution of Female Lengths",
     xlab = "Length (m)")

# simmulate Ratios
max_R_F <- 0.22 #maximum ntb ratio
r_F <- 0.2 #initial growth rate (same for males and females)

y_F <-  max_R_F * exp(r_F * x_F) / (1 + exp(r_F * x_F)) +
  rnorm(n, mean = 0, sd = 0.005)


# simulate male lengths
mean.L.M <- 12
max.L.M <- 17

x_M <- rnorm(n * 2, mean = mean.L.M, sd = 4) %>%
  subset(. >= min.L & . <= max.L.M)
  
hist(x_M)




# 2. visualize relationships
# visualize points
ggplot(data.frame(Length = x_F, Ratio = y_F), aes(x = Length, y = Ratio)) +
  geom_point(col = "blue") +
  labs(title = "Female Growth Curve",
       x = "Total Length (m)",
       y = "Nose-to-Body Ratio") +
  xlim(3.6, 17.4) +
  ylim(0.15, 0.27) +
  theme_classic()
# 3. Fit a bayesian mixture model for both curves
# 4. Fit bayesian mixture model with sex being hidden
# 5. Can the model correctly guess sex of initial data?