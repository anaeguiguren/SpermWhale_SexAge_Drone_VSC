# Simulation exercise
# This script simulates a set of whales to match the tip of
# snout-center of eye ratio graph in Figure 4 on Nishiwaki 1963.
# I used automeris.io to extract the points used to construct
# the two curves


# Load required packages
library(tidyverse)
library(rstan)
source("Scripts/functions.R")
set.seed(1991)

# 1. generate ratios (R) for male and females to match Nishiwaki-----
params.true <- read.csv("Data/nishiwaki_parameters.csv")
# parameters:

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

# Produce simulated observations of n-r ratios

fmax <- params.true$Value[1]
print(fmax) #maximum ntb ratio

fr <- params.true$Value[3]
print(fr) #initial growth rate (same for males and females)

y_F <-  fmax * exp(fr * x_F) / (1 + exp(fr * x_F)) +
  rnorm(n, mean = 0, sd = 0.005) #add noise


# simulate male lengths
mean.L.M <- 12
max.L.M <- 17


#uniform distribution of males, young adults should leave
x_M <- runif(n, min = min.L, max = max.L.M)


  
hist(x_M)

# simulate male ratios in a piecewise manner
mmax <- params.true$Value[2] #maximum ntb ratio
mr <- params.true$Value[4] # male growth rate post 6 m
chm <- 6 # size at which growth rate changes


base <- fmax * exp(fr * x_M) / (1 + exp(fr * x_M))

offset <- (x_M > chm) * mmax * (
  exp(mr * x_M) / (1 + exp(mr * x_M)) -
    exp(mr * chm) / (1 + exp(mr * chm))
)
noise <- rnorm(n, mean = 0, sd = 0.005)

y_M <- base + offset + noise



# 2. visualize simulated data----
df <- data.frame(Length = c(x_F, x_M),
                Ratio = c(y_F, y_M), 
                Sex = rep(c("F", "M"), each = 30))


# Create a sequence of lengths for the female lines
f_line <- data.frame(
  Length = seq(min.L, max.L.F, by = 0.2) 
)

f_line <- f_line %>%
  mutate(Ratio = fem_curve(Length, fr, fmax))

m_line <- data.frame(
  Length = seq(min.L, max.L.M, by = 0.2) 
)

m_line <- m_line %>%
  mutate(Ratio = mal_curve(Length, fr, fmax, mr, mmax, chm = 6))




# visualize points
fem_col <- "darkcyan"
mal_col <- "darkorange"

ggplot(df, aes(x = Length, y = Ratio, colour = Sex)) +
  geom_point(alpha = 0.6) +
  geom_line(data = f_line, aes(x = Length, y = Ratio),
            inherit.aes = FALSE, colour = fem_col) +
  geom_line(data = m_line, aes(x = Length, y = Ratio),
            inherit.aes = FALSE, colour = mal_col,) +
  scale_color_manual(values = c("F" = fem_col, "M" = mal_col)) +
  labs(title = "Simulated Growth Curves with Curves with known 
  parameters from Nishiwaki 1963",
       x = "Total Length (m)",
       y = "Nose-to-Body Ratio") +
  theme_classic()

ggsave("Figures/Nishiwaki_simmulation_fit.png", width = 8, height = 6)



# 3. Estimate Model Parameters using Least Square Optimization ----


#~~~~a. Find optimal model ----

fit_result <- optim_sex(data = df, chm = 6, pard0 = c(fr = 0.5, fmax = 0.5, mr = 0.5, mmax = 0.5), 
          weighted = F)

#extract estimated parameters:
params.est <- fit_result$params


#~~~~b. P point is female ----
df$Pr_female <-  f_probs(params = params.est, data = df)


ggplot(df, aes(x = Length, y = Ratio, color = Pr_female)) +
  geom_point(size = 2) +
  scale_color_gradient(high = fem_col, low = mal_col, name = "Posterior Pr (female)") +
  geom_line(data = f_line, aes(x = Length, y = Ratio),
            inherit.aes = FALSE, colour = fem_col) +
  geom_line(data = m_line, aes(x = Length, y = Ratio),
            inherit.aes = FALSE, colour = mal_col,) +
  theme_minimal()

#~~~~c. Find optimal prob cutoff----
library(ROCR)
library(PresenceAbsence)
#make binary sex column (F = 1, M = 0)

df <- df %>%
  mutate(
    Sex_bin = ifelse(Sex == "F", 1, 0)
  )


perf <- model_perf(bin_sex = df$Sex_bin, fem_probs = df$Pr_female)
thresh <- perf$threshold
perf$conmat
perf$true.pos # females identified as females
perf$true.neg # males identified as males

#~~~~d. Find optimal size cutoff----
df <- df %>%
  mutate(
    sex_model = ifelse(Pr_female >= thresh, 1, 0),
    correct_assign = abs(sex_model - Sex_bin)
  )


ggplot(df, aes(x = Length, y = Ratio, colour = as.factor(correct_assign))) +
  scale_colour_manual(values = c("blue", "red"))+
  geom_point(size = 2) +
  theme_classic()


# make 1 meter bins:
df <- df %>%
  mutate(
    bin = cut(Length, breaks = seq(floor(min(Length)), ceiling(max(Length)),
                                    by = 1), include.lowest = TRUE)
  )

# create bin dataframe:
bin_summary <- df %>%
  group_by(bin) %>%
  summarise(proportion_correct = mean(correct_assign==0), .groups = "drop")

bin_summary <- bin_summary %>%
  mutate(bin_mid = as.numeric(gsub("\\((.+),(.+)\\]", "\\1", levels(bin)[bin])) + 0.5)

# Plot the result
ggplot(bin_summary, aes(x = bin_mid, y = proportion_correct)) +
  geom_smooth() +
  geom_point() +
  labs(
    x = "X (1-meter bins)",
    y = "Proportion Correctly Assigned",
    title = "Proportion of Correct Sex Assignments by 1-meter Bin"
  ) +
  theme_minimal()
