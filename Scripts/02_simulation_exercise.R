# Simulation exercise
# This script simulates a set of whales to match the tip of
# snout-center of eye ratio graph in Figure 4 on Nishiwaki 1963.
# I used automeris.io to extract the points used to construct
# the two curves


# Load required packages
library(tidyverse)
library(rstan)
set.seed(1991)

# 1. generate ratios (R) for male and females to match Nishiwaki-----
params <- read.csv("Data/nishiwaki_parameters.csv")
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

# simmulate Ratios

max_R_F <- params$Value[1] ; print(max_R_F) #maximum ntb ratio
r_F <- params$Value[3] ; print(r_F) #initial growth rate (same for males and females)

y_F <-  max_R_F * exp(r_F * x_F) / (1 + exp(r_F * x_F)) +
  rnorm(n, mean = 0, sd = 0.005)


# simulate male lengths
mean.L.M <- 12
max.L.M <- 17


#uniform distribution of males, young adults should leave
x_M <- runif(n, min = min.L, max = max.L.M)


  
hist(x_M)

# simulate male ratios in a piecewise manner
max_R_M <- params$Value[2] #maximum ntb ratio
r_M <- params$Value[4] # male growth rate post 6 m
chm <- 6 # size at which growth rate changes


base <- max_R_F * exp(r_F * x_M) / (1 + exp(r_F * x_M))

offset <- (x_M > chm) * max_R_M * (
  exp(r_M * x_M) / (1 + exp(r_M * x_M)) -
    exp(r_M * chm) / (1 + exp(r_M * chm))
)
noise <- rnorm(n, mean = 0, sd = 0.005)

y_M <- base + offset + noise



# 2. visualize simulated data----
df = data.frame(Length = c(x_F, x_M),
                Ratio = c(y_F, y_M), 
                Sex = rep(c("F", "M"), each = 30))


fem_curve <- function(length, r_F = params$Value[3], max_R_F = params$Value[1]){
  max_R_F * exp(r_F * length) / (1 + exp(r_F * length))
}

mal_curve <- function(length, r_F = params$Value[3],
                      max_R_F = params$Value[1],
                      r_M = params$Value[4], 
                      chm = 6, max_R_M = params$Value[2]){
                base <- max_R_F * exp(r_F  * length) / (1 + exp(r_F * length))

                offset <- (length > chm) * max_R_M * (
                 exp(r_M * length) / (1 + exp(r_M * length)) -
    exp(r_M * chm) / (1 + exp(r_M * chm)))
Ratio <- base + offset
  return(Ratio)  
}





f_line <- data.frame(
  Length = seq(min.L, max.L.F, by = 0.2) 
)

f_line <- f_line %>%
  mutate(Ratio = fem_curve(Length))

m_line <- data.frame(
  Length = seq(min.L, max.L.M, by = 0.2) 
)

m_line <- m_line %>%
  mutate(Ratio = mal_curve(Length))




# visualize points
ggplot(df, aes(x = Length, y = Ratio, colour = Sex)) +
  geom_point() +
  geom_line(data = f_line, aes(x = Length, y = Ratio),
            inherit.aes = F, colour = 2) +
  geom_line(data = m_line, aes(x = Length, y = Ratio),
            inherit.aes = F, colour = 3) +
  labs(title = "Simulated Growth Curves",
       x = "Total Length (m)",
       y = "Nose-to-Body Ratio") +
  theme_classic()

ggsave("Figures/Nishiwaki_simmulation_fit.png", width = 8, height = 6)



# 3. Fit a bayesian mixture model for both curves
# 4. Fit bayesian mixture model with sex being hidden
# 5. Can the model correctly guess sex of initial data?