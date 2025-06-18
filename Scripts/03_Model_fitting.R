# Growth Curve Parameter Optimization
# Load necessary libraries and functions
source("Scripts/functions.R")

# 1. Load cleaned data ----
clean_data <- read.csv("Data/Processed_Data/id_morpho_output_clean_processed.csv") 

# Filter data ----
dat_HD <- clean_data %>%
  select(ID, Length = mean_TL, Length_SD = sd_TL,
         R = mean_ratio.HD, R_sd = sd_ratio.HD, suckled_ever, suckling_ever)%>%
  filter(!is.na(R_sd))

dat_HF <- clean_data %>%
  select(ID, Length = mean_TL, Length_SD = sd_TL,
         R = mean_ratio.HF, R_sd = sd_ratio.HF, suckled_ever, suckling_ever)%>%
  filter(!is.na(R_sd))


