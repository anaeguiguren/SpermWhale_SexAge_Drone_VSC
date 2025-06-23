#bootstraping:
data(iris)


library(boot)

# estimate the median and CIS for sepal.length, width, and spearman's correlation between the two
# 1. make a function with the data and indices (row numbers)


foo <- function(data, indices, cor.type){
  dt <- data[indices,]#subset data
  
  c(
    cor(dt[,1], dt[,2], method = cor.type),
    median(dt[,1]),
    median(dt[,2])
  )
}


#apply boot function:

set.seed(1991)
myBoot <-boot(iris, foo, R = 1000, cor.type = 's')
myBoot$t0 # observed values for full dataset


head(myBoot$t) #values obtained in each bootstrap


colMeans(myBoot$t)

## [1]  0.002546391 -0.013350000  0.007900000

plot(myBoot, index = 2)
boot.ci(myBoot, index = 1)#basic ci. 
boot.ci(myBoot, index = 2)#basic ci. 



true.altitude <- function(true.length, pixel.length, image.width){
  alpha = ifelse(image.width == 3840, yes = 0.000328, no = 
                   ifelse(image.width == 1920, yes = 0.000656, no = NA))
  t.a = (true.length/(pixel.length* alpha))
  return(t.a)
}





foo <- function(data, indices){
  dt <- data[indices,]#subset data
  m<-gls(true.altitude ~ altitude.fix,, data = dt, method = "ML", verbose = F)
  
  c(
    unname(coef(m)[1]),#intercept
    unname(coef(m)[2]),# slope
    sd(dt$true.altitude - dt$altitude.fix) #error
  )
}


myBoot <-boot(dat, foo, R = 1000)
myBoot$t0
colMeans(myBoot$t)



# bootstrap function:
bootstrap_length_ci <- function(mean_length, n_boot = 1000, conf = 0.95){
  #simulate errors for each bootstrap
  error_factor <- rnorm(n_boot, mean = 1 + error_mean, sd = error_sd)
  boot_lengths <- mean_length * error_factor
  mean_boot <- mean(boot_lengths)
  alpha <- (1 - conf) / 2
  length_ci <- quantile(boot_lengths, probs = c(alpha, 1 - alpha))
  out <- list(mean = mean_boot, ci_low = length_ci[1], ci_high = length_ci[2])
  return(out)
}

clean_data <- clean_data %>%
  rowwise() %>%
  mutate(
    boot = list(bootstrap_length_ci(mean_TL))
  ) %>%
  mutate(
    Length_mean_boot = boot$mean,
    Length_CI_Low = boot$ci_low,
    Length_CI_High = boot$ci_high
  ) %>%
  select(-boot) %>%
  ungroup()



ggplot(clean_data, aes(x = reorder(ID, mean_TL), y = mean_TL)) +
  geom_point(color = "blue", size = 2) +  # Original mean
  geom_errorbar(aes(ymin = Length_CI_Low, ymax = Length_CI_High),
                width = 0.2, color = "red") +  # Bootstrap CI
  geom_errorbar(aes(ymin = mean_TL - sd_TL, ymax = mean_TL + sd_TL),
                width = 0.1,
                color = "black", linetype = "dashed") +  # Original SD
  labs(
    x = "Whale ID",
    y = "Length (m)",
    title = "Whale Length Estimates with Bootstrap Confidence Intervals and Original SD"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
