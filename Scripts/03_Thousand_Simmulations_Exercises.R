#Thousand Simulations
source("Scripts/functions.R")
library("wacolors")
#Simulation parameters
n_sims <- 1000
n_whales <- 30

#Initialize storage matrices
# Rows = simulations, Columns = measurements
param_estimates <- matrix(NA, nrow = n_sims, ncol = 4)
colnames(param_estimates) <- c("fr", "fmax", "mr", "mmax")

performance_metrics <- matrix(NA, nrow = n_sims, ncol = 3)
colnames(performance_metrics) <- c("threshold",
                                   "true_pos_rate", "true_neg_rate")

#Define length bins 
length_bins <- seq(4, 17, by = 1)
n_bins <- length(length_bins) - 1

#Initialize bin-specific performance matrix
bin_performance <- matrix(NA, nrow = n_sims, ncol = n_bins)
colnames(bin_performance) <- paste(length_bins[-length(length_bins)],
                                   length_bins[-1], sep = "-")

#Read in known data:
params.true <- read.csv("Data/nishiwaki_parameters.csv")
# parameters:

n <- 30 # number of whales of each sex
min.L <- 4 # length at birth
max.L.F <- 11 # max length females
mean.L.F <- 8 # guestimated mean length of females
mean.L.M <- 12 # mean male length
max.L.M <- 17 #Max male length

fmax <- params.true$Value[1]
fr <- params.true$Value[3]
mmax <- params.true$Value[2] #maximum ntb ratio
mr <- params.true$Value[4] # male growth rate post 6 m
chm <- 6 # size at which growth rate changes

#Loop Through
for (i in 1:n_sims){
  #Females:
  x_F <- rnorm(n, mean = mean.L.F, sd = (max.L.F - mean.L.F) / 3) %>%
  #Truncate values to be between min and max length
  pmax(min.L) %>%
  pmin(max.L.F)

  y_F <-  fmax * exp(fr * x_F) / (1 + exp(fr * x_F)) +
  rnorm(n, mean = 0, sd = 0.005) #add noise

  #Males
  x_M <- runif(n, min = min.L, max = max.L.M)
  
  base <- fmax * exp(fr * x_M) / (1 + exp(fr * x_M))

  offset <- (x_M > chm) * mmax * (
  exp(mr * x_M) / (1 + exp(mr * x_M)) -
    exp(mr * chm) / (1 + exp(mr * chm))
)
noise <- rnorm(n, mean = 0, sd = 0.005)

y_M <- base + offset + noise

#Create simulated data frame:
df_sim <- data.frame(Length = c(x_F, x_M),
                Ratio = c(y_F, y_M), 
                Sex = rep(c("F", "M"), each = n_whales),
                Sex_bin = rep(c(1, 0), each = n_whales)
                )

 # Fit model
  fit_result <- tryCatch({
    optim_sex(data = df_sim, chm = 6, 
              pard0 = c(fr = 0.5, fmax = 0.5, mr = 0.5, mmax = 0.5),
              weighted = FALSE)
  }, error = function(e) NULL)
  
  if(!is.null(fit_result)) {
    # Store parameter estimates
    param_estimates[i,] <- unname(fit_result$params)
  }
    # Calculate probabilities
    df_sim$Pr_female <- f_probs(params = fit_result$params, data = df_sim)
    
    # Calculate performance metrics
    perf <- model_perf(bin_sex = df_sim$Sex_bin, fem_probs = df_sim$Pr_female)
    performance_metrics[i, ] <- c(
      perf$threshold,
      perf$true.pos,                          # true positive rate
      perf$true.neg                           # true negative rate
    )

    # Get right assignments for binned columns
    
    bin_perf <- perf_bins(data = df_sim, threshold = perf$threshold)

   # Match bins and assign values
    bin_names <- colnames(bin_performance)
    perf_bins_named <- setNames(rep(NA, length(bin_names)), bin_names)
    
    # Match bin names with values from bin_perf
    for(bin in bin_names) {
        match_idx <- which(bin_perf$length_bin == bin)
        if(length(match_idx) > 0) {
            perf_bins_named[bin] <- bin_perf$prop_right[match_idx]
        }
    }
    
    # Assign to performance matrix
    bin_performance[i, ] <- perf_bins_named
  
  # Progress indicator
  if(i %% 100 == 0) cat("Completed", i, "simulations\n")
}

# Save results
results <- list(
  parameters = as.data.frame(param_estimates),
  performance = as.data.frame(performance_metrics),
  true_params = c(fr = fr, fmax = fmax, mr = mr, mmax = mmax)
)
saveRDS(results, "Results/simulation_results.rds")

 
# Create summary plots
library(ggplot2)
library(tidyr)

# Summary plots:
library(ggdist)

#~~~a. Parameter estimates ----
#reshape data for plotting
true_params <- (results$true_params)

true_params <- data.frame(Parameter = names(true_params), True_value = unname(true_params))


params_long <- results$parameters %>%
  pivot_longer(cols = everything(), 
               names_to = "Parameter", 
               values_to = "Estimate") %>%
  mutate(Parameter = factor(Parameter, 
                            levels = c("fr", "fmax", "mr", "mmax")))

# add known parameter estimates:

params_long<-params_long %>%
  left_join(true_params, by = "Parameter")

# make plot for each parameter


ggplot(params_long, aes(x = Estimate, fill = Parameter, colour = Parameter)) +
  geom_density(alpha = 0.6)+
  geom_vline(aes(xintercept = True_value, colour = Parameter), linetype = "dashed")+
  facet_wrap(~Parameter, scales = "free")+
  scale_fill_wa_d("rainier")+
  scale_color_wa_d("rainier")+
  theme_minimal()+
  theme(legend.position = "none")


#~~~b. Curve shapes ----

# Create a sequence of lengths for the female lines
f_line <- data.frame(
  Simmulation = rep(seq(1, n_sims, by = 1), each = 36),
  Length = rep(seq(min.L, max.L.F, by = 0.2), n_sims) 
)

for(i in seq_along(n_sims)){
  
}

f_line <- f_line %>%
  mutate(Ratio = fem_curve(Length, fr = , fmax))

m_line <- data.frame(
  Length = seq(min.L, max.L.M, by = 0.2) 
)

m_line <- m_line %>%
  mutate(Ratio = mal_curve(Length, fr, fmax, mr, mmax, chm = 6))




  