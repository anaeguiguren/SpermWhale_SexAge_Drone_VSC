# This script estimates growth curve parameters from Nishiwaki 1963 data
# The data was extracted from Figure 4 using automeris.io
# Parameters will be used as priors in the Bayesian model

# Load required packages
library(tidyverse)
source("Scripts/functions.R")

# 1. Read and clean data----
m_dat <- read.csv("Data/Processed_Data/males_nishiwaki.csv", header = F)
f_dat <- read.csv("Data/Processed_Data/females_nishiwaki.csv", header = F)

# clean data and add sex column
m_dat <- m_dat %>%
  mutate(Length = V1,
         Ratio = V2 / 100,
         Sex = "Male") %>%
  select(Length, Ratio, Sex)

f_dat <- f_dat %>%
  mutate(Length = V1,
         Ratio = V2 / 100,
         Sex = "Female") %>%
  select(Length, Ratio, Sex)


all_dat <- bind_rows(m_dat, f_dat)

# 2. Specify Growth Curve functions----

# Function to fit logistic curve to females and little whales (under 6 m)
fit_logistic <- function(data) {
  # Fit nonlinear model
  nls(Ratio ~ fmax * exp(fr * Length) / (1 + exp(fr * Length)),
      data = data,
      start = list(fmax = 0.5, fr = 0.5),
      control = nls.control(maxiter = 100))
}

# Modified function to fit male growth after change point
fit_male_piecewise <- function(data, early_params, chm = 6) {
  # Keep only data after the change point
  late_data <- filter(data, Length > chm)

  # fit base curve based on female parameters
  base <- early_params["fmax"] * exp(early_params["fr"] * chm) /
    (1 + exp(early_params["fr"] * chm))


  # estimate parameters after change point

  late_model <- nls(Ratio ~ base +
                      mmax * (exp(mr * Length) / (1 + exp(mr * Length)) -
                                exp(mr * chm) / (1 + exp(mr * chm))),
                    data = late_data,
                    start = list(mmax = 0.5, mr = 0.5),
                    control = nls.control(maxiter = 2000))
  
  return(list(early_params = early_params,
              late_params = coef(late_model),
              base = base))
}



# Linear version of male piecewise function
fit_male_piecewise_linear <- function(data, early_params, chm = 6) {
  # Keep only data after the change point
  late_data <- filter(data, Length > chm)
  
  # fit base curve based on female parameters (stays the same)
  base <- early_params["fmax"] * exp(early_params["fr"] * chm) /
    (1 + exp(early_params["fr"] * chm))
  
  
  # estimate parameters after change point
  
  late_model <- nls(Ratio ~ base +
                      mr_l * (Length - chm),
                    data = late_data,
                    start = list(mr_l = 0.5),
                    control = nls.control(maxiter = 2000))
  
  return(list(early_params = early_params,
              late_params = coef(late_model),
              base = base))
}


# Fit separate models for males and females

female_model <- fit_logistic(filter(all_dat, Sex == "Female" | Length <= 6))

# get parameter estimates

female_params <- coef(female_model)

#fit the male model (exponential)

male_model <- fit_male_piecewise(filter(all_dat, Sex == "Male" & Length > 6),
                                 early_params = female_params)


#fit the male model(linear)

male_model_lin <- fit_male_piecewise_linear(filter(all_dat, Sex == "Male" & Length > 6),
                                 early_params = female_params)




# Extract parameters
male_late_params <- male_model$late_params

male_late_params_linear <- male_model_lin$late_params

# Print parameters
cat("Female parameters:\n")
print(female_params)
cat("\nMale late parameters:\n")
print(male_late_params)
cat("\nMale late parameters (linear):\n")
print(male_late_params_linear)




# 3. Visualize fits----
# Create prediction data
length_seq <- seq(4, 17, by = 0.1)

# Predict curves
female_pred <- female_params["fmax"] * exp(female_params["fr"] * length_seq) /
  (1 + exp(female_params["fr"] * length_seq))

# male (exponential)
male_pred <- ifelse(length_seq <= 6,
                    # Early growth (same as females)
                    female_params["fmax"] * exp(female_params["fr"] * length_seq) / 
                      (1 + exp(female_params["fr"] * length_seq)),
                    # Late growth (base + additional growth)
                    male_model$base +
                      male_late_params["mmax"] * (
                        exp(male_late_params["mr"] * length_seq) /
                          (1 + exp(male_late_params["mr"] * length_seq)) -
                          exp(male_late_params["mr"] * 6) / 
                            (1 + exp(male_late_params["mr"] * 6))
                      ))


# male (linear)

male_pred_linear <- ifelse(length_seq <= 6,
                    # Early growth (same as females)
                    female_params["fmax"] * exp(female_params["fr"] * length_seq) / 
                      (1 + exp(female_params["fr"] * length_seq)),
                    # Late growth (base + additional growth)
                    
                    male_model_lin$base +
                      male_late_params_linear["mr_l"] * (length_seq - 6)
                      )

# Create prediction dataframe
pred_df <- data.frame(
  Length = rep(length_seq, 3),
  Ratio = c(female_pred, male_pred, male_pred_linear),
  Sex = rep(c("Female", "Male", "Male_l"), each = length(length_seq))
)

# make curve variable
pred_df$Curve <- factor(ifelse(pred_df$Sex == "Male_l", "linear", "exponential"))


# Plot
fem_col <- "darkcyan"
mal_col <- "darkorange"

ggplot(all_dat, aes(x = Length, y = Ratio, color = Sex)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_line(data = pred_df, aes(x = Length, y = Ratio, color = Sex, 
                                linetype = Curve)) +
  labs(title = "Nishiwaki 1963 Growth Curves",
       x = "Total Length (m)",
       y = "Nose-to-Body Ratio") +
  scale_color_manual(
    values = c(
      "Female" = fem_col,
      "Male" = mal_col, 
      "Male_l" = mal_col),
    labels = c(
      "Female" = "female",
      "Male"   = "male",
      "Male_l" = "male"
    )
    ) +

  theme_classic()

# 4. Calculate asymptotic ratios and growth rates----
# These will be used as priors in the  model
max_ratio_F <- female_params["fmax"]
max_ratio_M <- male_late_params["mmax"]
growth_rate_F <- female_params["fr"]
growth_rate_M <- male_late_params["mr"]
growth_rate_M_linear <- male_late_params_linear["mr_l"]

# Save parameters to a file
params <- data.frame(
  Parameter = c("fmax", "mmax", "fr", "mr", "mr_l"),
  Value = c(max_ratio_F, max_ratio_M, growth_rate_F, growth_rate_M, growth_rate_M_linear)
)

write.csv(params, "Data/Processed_Data/nishiwaki_parameters.csv", row.names = FALSE)
