# Data Wrangling Script
# This script processes the raw morphometric measurements

# Load required packages
library(tidyverse)

# Read raw data
dat <- read.csv("Data/id.morpho.output.clean.csv", T)


# Summarize by individual ID
dat_id <- dat %>%
  group_by(ID) %>%
  summarise(
    mean.TL = mean(TL.m, na.rm = TRUE),
    sd.TL = sd(TL.m, na.rm = TRUE),
    mean.Ratio.HF = mean(ratio.HF, na.rm = TRUE),
    sd.Ratio.HF = sd(ratio.HF, na.rm = TRUE),
    mean.Ratio.HD = mean(ratio.HD, na.rm = TRUE),
    sd.Ratio.HD = sd(ratio.HD, na.rm = TRUE),
    is_male = any(notes == "male", na.rm = TRUE)  # Flag known males
  )

# Create a subset of the data with only the relevant columns
dat_subset <- dat_id %>%
  select(
    ID,
    L = mean.TL,
    R.hf = mean.Ratio.HF, sd.R.hf = sd.Ratio.HF,
    R.hd = mean.Ratio.HD, sd.R.hd = sd.Ratio.HD
  )



# Save processed data
write.csv(dat_subset, "Data/Processed_Data/id_morpho_output_clean_processed.csv", row.names = FALSE)
