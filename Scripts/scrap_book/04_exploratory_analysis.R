# Exploratory Data Analysis
# This script creates exploratory visualizations and summary statistics

# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)  # for arranging multiple plots

# Set theme for consistent plotting
theme_set(theme_bw(base_size = 12))

# Load cleaned data
clean_data <- read.csv("Data/Processed_Data/id_morpho_output_clean_processed.csv")
head(clean_data)
cat("\nNumber of individuals:", nrow(clean_data))

# Create directory for plots if it doesn't exist
dir.create("Figures", showWarnings = TRUE)

# 1. Scatter plot of total length vs nose-to-body ratios
p1 <- ggplot(clean_data, aes(x = L, y = R.hf)) +
  geom_point(aes(size = 1 / sd.R.hf), alpha = 0.6) +
  #geom_smooth(method = "loess", se = TRUE) +
  labs(x = "Total Length (m)",
       y = "Head-to-Fluke Ratio",
       size = "Measurement Precision") +
  scale_size_continuous(name = "Precision\n(1/SD)") +
  theme_bw() +
  theme(legend.position = "right")

# Save the plot
ggsave("Figures/length_vs_ratio.png", p1, width = 8, height = 6)
p1

# 3. Precision analysis
# Box plots of standard deviations
p4 <- ggplot(clean_data, aes(y = sd.R.hf)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(y = "Standard Deviation of Head-to-Fluke Ratio")

ggsave("Figures/measurement_precision.png", p4, width = 6, height = 4)

# Print key findings
cat("\nKey Statistics:\n")
cat("Mean Length:", round(mean(clean_data$L, na.rm = TRUE), 2), "m\n")
cat("Length Range:", round(min(clean_data$L, na.rm = TRUE), 2), "to",
    round(max(clean_data$L, na.rm = TRUE), 2), "m\n")
cat("Mean H-F Ratio:", round(mean(clean_data$R.hf, na.rm = TRUE), 3), "\n")
cat("H-F Ratio Range:", round(min(clean_data$R.hf, na.rm = TRUE), 3), "to",
    round(max(clean_data$R.hf, na.rm = TRUE), 3), "\n")
cat("Mean measurement SD:",
    round(mean(clean_data$sd.R.hf, na.rm = TRUE), 3), "\n")
