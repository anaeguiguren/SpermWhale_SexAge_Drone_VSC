# Growth Curve Parameter Optimization
# Load necessary libraries and functions
source("Scripts/functions.R")
set.seed(1234567)

# 1. Load cleaned data ----
clean_data <- read.csv("Data/Processed_Data/id_morpho_output_clean_processed.csv") 


# Propagate length estimate error associated with drone-measurement error:
error_mean <- 0.0012 # 0.12% mean
error_sd <- 0.0315 # 3.15% sd

# bootstrap function:
bootstrap_length_ci <- function(mean_length, n_boot = 1000, conf = 0.95){
  #simulate errors for each bootstrap
  error_factor <- rnorm(n_boot, mean = 1 + error_mean, sd = error_sd)
  boot_lengths <- mean_length * error_factor
  mean_boot <- mean(boot_lengths)
  alpha <- (1 - conf) / 2
   length_ci <- quantile(boot_lengths, probs = c(alpha, 1 - alpha))
  out <- list(mean = mean_boot, ci_low = length_ci[1], ci_high = length_ci[2])
  return(out)
}

clean_data <- clean_data %>%
  rowwise() %>%
  mutate(
    boot = list(bootstrap_length_ci(mean_TL))
  ) %>%
  mutate(
    Length_mean_boot = boot$mean,
    Length_CI_Low = boot$ci_low,
    Length_CI_High = boot$ci_high
  ) %>%
  select(-boot) %>%
  ungroup()



ggplot(clean_data, aes(x = reorder(ID, mean_TL), y = mean_TL)) +
  geom_point(color = "blue", size = 2) +  # Original mean
  geom_errorbar(aes(ymin = Length_CI_Low, ymax = Length_CI_High),
                width = 0.2, color = "red") +  # Bootstrap CI
  geom_errorbar(aes(ymin = mean_TL - sd_TL, ymax = mean_TL + sd_TL),
                width = 0.1,
                color = "black", linetype = "dashed") +  # Original SD
  labs(
    x = "Whale ID",
    y = "Length (m)",
    title = "Whale Length Estimates with Bootstrap Confidence Intervals and Original SD"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Filter data ----
# head to dorsal fin: 
dat_HD <- clean_data %>%
  select(ID, Length = mean_TL, Length_SD = sd_TL,
         R = mean_ratio.HD, R_sd = sd_ratio.HD,
         suckled_ever, suckling_ever, n_photos) %>%
  filter(n_photos > 2 & !is.na(R_sd))

dim(dat_HD)

dat_HF <- clean_data %>%
  select(ID, Length = mean_TL, Length_SD = sd_TL,
         R = mean_ratio.HF, R_sd = sd_ratio.HF,
         suckled_ever, suckling_ever, n_photos) %>%
  filter(n_photos > 2 & !is.na(R_sd))
dim(dat_HF)


# Propagate length estimate error associated with drone-measurement error:
error_mean <- 0.0012 # 0.12% mean
error_sd <- 0.0315 # 3.15% sd

# bootstrap function:
bootstrap_length_ci <- function(mean_length, n_boot = 1000, conf = 0.95){
  #simulate errors for each bootstrap
  error_factor <- rnorm(n_boot, mean = 1 + error_mean, sd = error_sd)
  boot_lengths <- mean_length * error_factor
  mean_boot <- mean(boot_lengths)
  length_ci <- quantile(boot_lengths, probs = c(0.025, 0.972))
  out <-list(mean = mean_boot, length_ci)
  return(out)
}

# estimate mean and bootstrapped lengths:

