# Scripts/test_03_model_fitting.R

library(testthat)
library(dplyr)

# Mock data similar to clean_data
mock_data <- data.frame(
       ID = 1:10,
       mean_TL = seq(10, 19, by = 1),
       sd_TL = rep(0.5, 10),
       mean_ratio.HD = seq(1, 2, length.out = 10),
       sd_ratio.HD = rep(0.1, 10),
       mean_ratio.HF = seq(1.5, 2.5, length.out = 10),
       sd_ratio.HF = rep(0.1, 10),
       suckled_ever = sample(c(TRUE, FALSE), 10, replace = TRUE),
       suckling_ever = sample(c(TRUE, FALSE), 10, replace = TRUE),
       n_photos = sample(1:5, 10, replace = TRUE)
)

# Simulate the code's filtering
dat_HD <- mock_data %>%
       select(ID, Length = mean_TL, Length_SD = sd_TL,
                             R = mean_ratio.HD, R_sd = sd_ratio.HD,
                             suckled_ever, suckling_ever, n_photos) %>%
       filter(!is.na(R_sd))

dat_HF <- mock_data %>%
       select(ID, Length = mean_TL, Length_SD = sd_TL,
                             R = mean_ratio.HF, R_sd = sd_ratio.HF,
                             suckled_ever, suckling_ever, n_photos) %>%
       filter(!is.na(R_sd))

# Bootstrapping function to propagate drone error
propagate_drone_error <- function(df, n_boot = 1000, error_mean = 0.12, error_sd = 3.15) {
       boot_means <- replicate(n_boot, {
              error_factor <- rnorm(nrow(df), mean = error_mean/100, sd = error_sd/100)
              length_boot <- df$Length * (1 + error_factor)
              r_boot <- df$R * (1 + error_factor)
              c(mean(length_boot), mean(r_boot))
       })
       length_ci <- quantile(boot_means[1,], probs = c(0.025, 0.975))
       r_ci <- quantile(boot_means[2,], probs = c(0.025, 0.975))
       list(
              Length_CI = length_ci,
              R_CI = r_ci,
              Length_mean = mean(boot_means[1,]),
              R_mean = mean(boot_means[2,])
       )
}

test_that("propagate_drone_error returns correct structure", {
       res <- propagate_drone_error(dat_HD, n_boot = 100)
       expect_true(is.list(res))
       expect_named(res, c("Length_CI", "R_CI", "Length_mean", "R_mean"))
       expect_length(res$Length_CI, 2)
       expect_length(res$R_CI, 2)
})

test_that("propagate_drone_error CIs contain true means", {
       res <- propagate_drone_error(dat_HD, n_boot = 1000)
       expect_true(mean(dat_HD$Length) >= res$Length_CI[1] && mean(dat_HD$Length) <= res$Length_CI[2])
       expect_true(mean(dat_HD$R) >= res$R_CI[1] && mean(dat_HD$R) <= res$R_CI[2])
})

test_that("propagate_drone_error works for dat_HF", {
       res <- propagate_drone_error(dat_HF, n_boot = 100)
       expect_true(is.list(res))
       expect_length(res$Length_CI, 2)
       expect_length(res$R_CI, 2)
})