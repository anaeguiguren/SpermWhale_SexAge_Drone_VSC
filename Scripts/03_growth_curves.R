# Growth Curve Parameter Optimization
# This script implements a Bayesian mixture model for sex-specific growth curves

# Load required packages
library(tidyverse)
library(rstan)
library(brms)
library(bayesplot)
library(tidybayes)

# Load cleaned data
clean_data <- read.csv("Data/Processed_Data/id_morpho_output_clean_processed.csv")

# Prepare data for stan
model_data <- clean_data %>%
  filter(!is.na(R.hf), !is.na(L), !is.na(sd.R.hf)) %>%  # Remove missing values
  select(L, R.hf, sd.R.hf) 


# Prepare data for Stan
stan_data <- list(
  N = nrow(model_data),
  L = model_data$L,
  R = model_data$R.hf,
  sd_R = model_data$sd.R.hf
)

# Create directory for Stan models if it doesn't exist
dir.create("stan_models", showWarnings = FALSE)

# Fit the model
fit <- stan(
  file = "Stan_Models/growth_curve.stan",
  data = stan_data,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = parallel::detectCores()-1,
  control = list(adapt_delta = 0.95)
)

# print summaries
print(fit)

posterior <- as.matrix(fit)


mcmc_areas(posterior)
