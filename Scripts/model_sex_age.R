# Load libraries
library(tidyverse)

# Load data
dat <- read.csv("Data/id.morpho.output.clean.csv")

# inspect data
head(dat)

# summarize by ID:

dat_id <- dat %>%
  group_by(ID) %>%
  summarise(
    mean.TL = mean(TL.m, na.rm = TRUE),
    sd.TL = sd(TL.m, na.rm = TRUE),
    mean.Ratio.HF = mean(ratio.HF, na.rm = TRUE),
    sd.Ratio.HF = sd(ratio.HF, na.rm = TRUE),
    mean.Ratio.HD = mean(ratio.HD, na.rm = TRUE),
    sd.Ratio.HD = sd(ratio.HD, na.rm = TRUE)
  )

# Create a subset of the data with only the relevant columns
dat_subset <- dat_id %>%
  select(
    L = mean.TL,
    R.hf = mean.Ratio.HF, sd.R.hf = sd.Ratio.HF,
    R.hd = mean.Ratio.HD, sd.R.hd = sd.Ratio.HD
  )

# 
