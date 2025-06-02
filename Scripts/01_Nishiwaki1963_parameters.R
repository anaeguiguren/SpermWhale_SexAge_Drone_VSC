# This script estimates growth curve parameters from Nishiwaki 1963 data
# The data was extracted from Figure 4 using automeris.io
# Parameters will be used as priors in the Bayesian model

# Load required packages
library(tidyverse)

# 1. Read and clean data----
m_dat <- read.csv("Data/males_nishiwaki.csv", header = F)
f_dat <- read.csv("Data/females_nishiwaki.csv", header = F)

# clean data and add sex column
m_dat <- m_dat %>%
  mutate(Length = V1, 
         Ratio = V2/100, 
         Sex = "Male") %>%
  select(Length, Ratio, Sex)

f_dat <- f_dat %>%
  mutate(Length = V1, 
         Ratio = V2/100, 
         Sex = "Female") %>%
  select(Length, Ratio, Sex)

# Combine data
all_dat <- rbind(m_dat, f_dat)



# 2. Fit logistic growth curves----

# Function to fit logistic curve to females and little whales (under 6 m)
fit_logistic <- function(data) {
  # Fit nonlinear model
  model <- nls(Ratio ~ max_R * exp(r * Length) / (1 + exp(r * Length)),
               data = data,
               start = list(max_R = 0.2, r = 0.2),
               control = nls.control(maxiter = 100))
  return(model)
}

# Modified function to fit male growth after change point
fit_male_piecewise <- function(data, early_params, chm = 6) {
  # Keep only data after the change point
  late_data <- filter(data, Length > chm)

  # fit base curve based on female parameters
  base <- early_params["max_R"] * exp(early_params["r"] * chm) /
    (1 + exp(early_params["r"] * chm))


  # estimate parameters after change point

  late_model <- nls(Ratio ~ base +
                      max_R_M * (exp(r_M * Length)/ (1 + exp(r_M * Length)) - 
                                   exp(r_M * chm)/ (1 + exp(r_M * chm))),
                    data = late_data,
                    start = list(max_R_M = 0.3, r_M = 0.2),
                    control = nls.control(maxiter = 200))
  return(list(early_params = early_params,
              late_params = coef(late_model),
              base = base))
}

# Fit separate models for males and females

female_model <- fit_logistic(filter(all_dat, Sex == "Female" | Length <= 6))
# get parameter estimates
female_params <- coef(female_model)

male_model <- fit_male_piecewise(filter(all_dat, Sex == "Male" & Length > 6),
                                 early_params = female_paramss)




# Extract parameters
male_late_params <- male_model$late_params

# Print parameters
cat("Female parameters:\n")
print(female_params)
cat("\nMale late parameters:\n")
print(male_late_params)

# 3. Visualize fits----
# Create prediction data
length_seq <- seq(4, 17, by = 0.1)

# Predict curves
female_pred <- female_params["max_R"] * exp(female_params["r"] * length_seq) / 
  (1 + exp(female_params["r"] * length_seq))

male_pred <- ifelse(length_seq <= 6,
                   # Early growth (same as females)
                   female_params["max_R"] * exp(female_params["r"] * length_seq) / 
                   (1 + exp(female_params["r"] * length_seq)),
                   # Late growth (base + additional growth)
                   male_model$base + 
                   male_late_params["max_R_M"] * (
                     exp(male_late_params["r_M"] * length_seq) / 
                     (1 + exp(male_late_params["r_M"] * length_seq)) -
                     exp(male_late_params["r_M"] * 6) / 
                     (1 + exp(male_late_params["r_M"] * 6))
                   ))

# Create prediction dataframe
pred_df <- data.frame(
  Length = rep(length_seq, 2),
  Ratio = c(female_pred, male_pred),
  Sex = rep(c("Female", "Male"), each = length(length_seq))
)

# Plot
ggplot(all_dat, aes(x = Length, y = Ratio, color = Sex)) +
  geom_point(alpha = 0.6) +
  geom_line(data = pred_df, aes(x = Length, y = Ratio, color = Sex)) +
  labs(title = "Nishiwaki 1963 Growth Curves",
       x = "Total Length (m)",
       y = "Nose-to-Body Ratio") +
  theme_classic()

# Save plot
ggsave("Figures/nishiwaki_fits.png", width = 8, height = 6)

# 4. Calculate asymptotic ratios and growth rates----
# These will be used as priors in the Bayesian model
max_ratio_F <- female_params["max_R"]
max_ratio_M <- male_late_params["max_R_M"]
growth_rate_F <- female_params["r"]
growth_rate_M <- male_late_params["r_M"]

# Save parameters to a file
params <- data.frame(
  Parameter = c("max_ratio_F", "max_ratio_M", "growth_rate_F", "growth_rate_M"),
  Value = c(max_ratio_F, max_ratio_M, growth_rate_F, growth_rate_M)
)

write.csv(params, "Data/nishiwaki_parameters.csv", row.names = FALSE)
