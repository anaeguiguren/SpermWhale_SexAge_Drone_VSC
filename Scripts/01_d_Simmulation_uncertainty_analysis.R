# Effect of measurement uncertainty on sex classification
# This script examines how different levels of measurement uncertainty
# affect our ability to correctly classify whale sex

# Load required libraries
library(tidyverse)  # for data manipulation and plotting
library(purrr)      # for mapping functions
library(dplyr)      # for data manipulation

library(ggplot2)
library(wacolors)
source("Scripts/functions.R")

# Parameters from Nishiwaki
params.true <- read.csv("Data/nishiwaki_parameters.csv")

# Simulation parameters
n_sims <- 100            # simulations per uncertainty level
n_whales <- 30           # whales per sex
n_measurements <- 3      # repeated measurements per whale
min.L <- 4              # minimum length
max.L.F <- 11          # max female length
max.L.M <- 17          # max male length
mean.L.F <- 8          # mean female length
mean.L.M <- 12         # mean male length
chm <- 6               # change point for male growth
biological_sd <- 0.005  # biological variation in length-ratio relationship

# True parameters from Nishiwaki
fmax <- params.true$Value[1]
fr <- params.true$Value[3]
mmax <- params.true$Value[2]
mr <- params.true$Value[4]

# Define uncertainty levels to test (SD of measurements)
uncertainty_levels <- seq(0.001, 0.02, length.out = 20)

# Function to generate repeated measurements with uncertainty

results <- list()

# Initial parameter guesses (same as in functions.R)
pard0 <- c(0.2, 0.2, 0.02, 0.2)  # fr, fmax, mr, mmax

for(i in seq_along(uncertainty_levels)) {
  uncertainty_sd <- uncertainty_levels[i]
  sim_results <- list()
  
  for(sim in 1:n_sims) {
    # Generate dataset with current uncertainty level
    data <- generate_dataset(uncertainty_sd, params.true)
    
    # Weight by inverse variance of measurements
    data$SD_Ratio <- data$sd_R  # Match name expected by optim_sex
    
    #group by ID:
    
    
    data.ID<- data %>%
      group_by(ID) %>%
      summarise(Length = first(Length), True_ratio = first(true_ratio),
                Ratio = mean(Ratio), Sex = first(Sex), SD_Ratio = first(sd_R))%>%
      mutate(upper = Ratio + SD_Ratio, lower = Ratio - SD_Ratio)
    #remove to mak sure I'm using the right data frame
    rm(data)
    
    
    # Use optim_sex to fit parameters with weighted regression
    fit <- try({
      optim_sex(data.ID, chm = chm, pard0 = pard0, weighted = FALSE)
    }, silent = TRUE)
    
    if(!inherits(fit, "try-error")) {
      # Calculate posterior probabilities of being female
      data.ID$fem_probs <- f_probs(fit$params, data.ID, chm = chm)
      
      # Find optimal threshold and get classification performance
      perf <- model_perf(bin_sex = (data.ID$Sex == "F"), fem_probs = data.ID$fem_probs)
      
      # Calculate accuracy using optimal threshold
      data.ID$predicted_sex <- ifelse(data.ID$fem_probs >= perf$threshold, "F", "M")
      accuracy <- mean(data.ID$predicted_sex == data.ID$Sex)
      
      # Store results
      sim_results[[sim]] <- data.frame(
        uncertainty = uncertainty_sd,
        simulation = sim,
        accuracy = accuracy,
        threshold = perf$threshold,
        true_pos = perf$true.pos,
        true_neg = perf$true.neg,
        fr = fit$params[1],
        fmax = fit$params[2],
        mr = fit$params[3],
        mmax = fit$params[4]
      )
    }
  }
  
  # Combine results for this uncertainty level
  results[[i]] <- do.call(rbind, sim_results)
  
  # Print progress
  cat(sprintf("Completed uncertainty level %d of %d\n", i, length(uncertainty_levels)))
}

# Combine all results
final_results <- do.call(rbind, results)

# Save results
write.csv(final_results, "uncertainty_analysis_results.csv", row.names = FALSE)

# Enhanced visualization with uncertainty bands
ggplot(final_results, aes(x = uncertainty)) +
  geom_point(aes(y = accuracy), alpha = 0.1) +
  geom_smooth(aes(y = accuracy), method = "loess", se = TRUE) +
  geom_smooth(aes(y = true_pos), method = "loess", se = TRUE, color = "red") +
  geom_smooth(aes(y = true_neg), method = "loess", se = TRUE, color = "blue") +
  labs(x = "Measurement Uncertainty (SD)",
       y = "Performance Metrics",
       title = "Impact of Measurement Uncertainty on Classification Performance",
       subtitle = "Black: Overall Accuracy, Red: True Positive Rate, Blue: True Negative Rate") +
  theme_minimal()




ggplot(final_results, aes(x = factor(uncertainty), y = accuracy)) +
  geom_boxplot()+
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "red")+
  scale_y_continuous(limits = c(0,1))



ggplot(final_results, aes(x = factor(uncertainty), y = true_pos)) +
  geom_boxplot()+
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "red")+
  
  scale_y_continuous(limits = c(0,1))
