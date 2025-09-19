#Thousand Simulations
source("Scripts/functions.R")
library("wacolors")
#Simulation parameters
n_sims <- 10000
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
  true_params = c(fr = fr, fmax = fmax, mr = mr, mmax = mmax),
  bin_performance = as.data.frame(bin_performance)
)
saveRDS(results, "Results/simulation_results.rds")

 
# Create summary plots
library(ggplot2)
library(tidyr)

# Summary plots:
library(ggdist)

#~~~a. Parameter estimates ----
#reshape data for plotting
parameters <- results$parameters
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


p1 <- ggplot(params_long,
             aes(x = Estimate, fill = Parameter, colour = Parameter)) +
  geom_density(alpha = 0.6) +
  geom_vline(aes(xintercept = True_value, colour = Parameter),
             linetype = "dashed") +
  facet_wrap(~Parameter, scales = "free", ncol = 4) +
  scale_fill_wa_d("rainier") +
  scale_color_wa_d("rainier") +
  labs(title = "Posterior distribution of parameter estimates from simmulated data", 
       xlab = "Parameter estimate",
       ylab = "density") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("Figures/simmulation_posterior_parameter_estimates.png",
       p1, width = 10, height = 3)

#~~~b. Curve shapes ----

# Create a sequence of lengths for the female lines
f_line <- expand.grid(
  Length = seq(min.L, max.L.F, by = 0.2), 
  sim = 1:n_sims
) %>%
as.data.frame()

# Add estimated ratios with parameters for each simulation
f_line <- f_line %>%
  mutate(
    fr = rep(param_estimates[,1], each = length(seq(min.L, max.L.F, by = 0.2))),
    fmax = rep(param_estimates[,2], each = length(seq(min.L, max.L.F, by = 0.2))),
    Ratio = fem_curve(Length, fr, fmax)
  )

# Male curves
m_line <- expand.grid(
  Length = seq(min.L, max.L.M, by = 0.2),
  sim = 1:n_sims
) %>%
  as.data.frame()

# Add estimated ratios using parameters from each simulation
m_line <- m_line %>%
  mutate(
    fr = rep(param_estimates[,1], each = length(seq(min.L, max.L.M, by = 0.2))),
    fmax = rep(param_estimates[,2], each = length(seq(min.L, max.L.M, by = 0.2))),
    mr = rep(param_estimates[,3], each = length(seq(min.L, max.L.M, by = 0.2))),
    mmax = rep(param_estimates[,4], each = length(seq(min.L, max.L.M, by = 0.2))),
    Ratio = mal_curve(Length, fr, fmax, mr, mmax, chm = 6)
  )

# Plot curves with uncertainty
p2<- ggplot() +
  # Add simulated curves with transparency
  geom_line(data = f_line,
            aes(x = Length, y = Ratio, group = sim),
            alpha = 0.03, color = "darkcyan") +
  geom_line(data = m_line,
            aes(x = Length, y = Ratio, group = sim),
            alpha = 0.03, color = "darkorange") +
  # Add true curves
  geom_line(data = data.frame(Length = seq(min.L, max.L.F, by = 0.2)) %>%
              mutate(Ratio = fem_curve(Length, fr, fmax)),
            aes(x = Length, y = Ratio),
            color = "darkcyan", linewidth = 1) +
  geom_line(data = data.frame(Length = seq(min.L, max.L.M, by = 0.2)) %>%
              mutate(Ratio = mal_curve(Length, fr, fmax, mr, mmax, chm = 6)),
            aes(x = Length, y = Ratio),
            color = "darkorange", linewidth = 1) +
  theme_classic() +
  labs(title = "Growth Curves with Parameter Uncertainty",
       x = "Length (m)",
       y = "Nose-to-Body Ratio")


ggsave("Figures/simmulation_curves.png",
       p2, width = 10, height = 6)



#~~~b. Probability of assignment distribution  ----
bin_performance <- results$bin_performance



# convert bin performance matrix to long format for plotting:
bin_performance_long <- as.data.frame(bin_performance) %>%
  mutate(sim = 1:n_sims) %>%
  pivot_longer(cols = -sim,
    names_to = "length_bin",
    values_to = "proportion_correct")%>%
  # Add proper ordering of bins
  mutate(length_bin = factor(length_bin, 
                            levels = paste(seq(4, 16), 
                                         seq(5, 17), 
                                         sep = "-")))

# Extract bin midpoints for plotting
bin_midpoints <- as.numeric(gsub("(.+)-(.+)", "\\1", 
                                unique(bin_performance_long$length_bin))) + 0.5

# Create summary statistics
bin_stats <- bin_performance_long %>%
  group_by(length_bin) %>%
  summarise(
    mean_prop = mean(proportion_correct, na.rm = TRUE),
    sd_prop = sd(proportion_correct, na.rm = TRUE),
    lower_ci = quantile(proportion_correct, 0.05, na.rm = TRUE),
    upper_ci = quantile(proportion_correct, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(bin_mid = bin_midpoints)

# plot
p3 <- ggplot() +
  # Add mean points
  geom_point(data = bin_stats,
             aes(x = length_bin, y = mean_prop),
             color = "darkblue",
             size = 2) +
  # Add error bars
  geom_errorbar(data = bin_stats,
                aes(x = length_bin, 
                    ymin = lower_ci, 
                    ymax = upper_ci),
                width = 0.2,
                color = "darkblue") +
  # Add threshold (95%)
  geom_hline(yintercept = 0.95, linetype = "dashed")+
  # Add styling
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Length Bin (m)",
       y = "Proportion Correctly Assigned",
       title = "Distribution of Classification Accuracy by Length Bin",
       subtitle = "Across 1000 simulations") +
  scale_y_continuous(limits = c(0, 1))

ggsave("Figures/simmulation_classification_accuracy_distribution.png",
       p3, width = 10, height = 6)
