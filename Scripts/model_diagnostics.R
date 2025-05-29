# Add diagnostic plots
mcmc_trace(fit, pars = c("p_female", "f_asymp", "f_k", "f_mid", "m_init", "m_slope", "threshold"))
ggsave("Figures/mcmc_traces.png", width = 12, height = 8)

# Extract parameters and credible intervals
parameters <- rstan::extract(fit)

# Calculate posterior means
post_means <- data.frame(
  parameter = c("p_female", "f_asymp", "f_k", "f_mid", "m_init", "m_slope", "threshold"),
  mean = c(
    mean(parameters$p_female),
    mean(parameters$f_asymp),
    mean(parameters$f_k),
    mean(parameters$f_mid),
    mean(parameters$m_init),
    mean(parameters$m_slope),
    mean(parameters$threshold)
  )
)

# Save results
saveRDS(fit, "stan_models/growth_curves_fit.rds")
write.csv(post_means, "Results/parameter_estimates.csv", row.names = FALSE)

# Extract individual probabilities
p_female_estimates <- data.frame(
  ID = clean_data$ID[!is.na(clean_data$R.hf) & !is.na(clean_data$L)],
  Length = clean_data$L[!is.na(clean_data$R.hf) & !is.na(clean_data$L)],
  Ratio = clean_data$R.hf[!is.na(clean_data$R.hf) & !is.na(clean_data$L)],
  p_female = colMeans(parameters$p_female_ind)
)

write.csv(p_female_estimates, "Results/individual_sex_probabilities.csv", row.names = FALSE)

# Print summary
print(summary(fit, pars = c("p_female", "f_asymp", "f_k", "f_mid", "m_init", "m_slope", "threshold"))$summary)
