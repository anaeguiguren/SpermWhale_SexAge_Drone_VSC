# Growth Curve Parameter Optimization
# Load necessary libraries and functions
source("Scripts/functions.R")
set.seed(1234567)

# 1. Load cleaned data ----
#mean metrics per individual:
clean_data <- read.csv("Data/Processed_Data/id_morpho_output_clean_processed.csv") 

#nishiwaki parameters from tip of snout to center of eye
nish <- read.csv("Data/nishiwaki_parameters.csv", header = T)



# 2. Subset datasets ----
# head to dorsal fin: 
dat_HD <- clean_data %>%
  select(ID, Length = mean_TL, Length_SD = sd_TL,
         Ratio = mean_ratio.HD, R_sd = sd_ratio.HD,
         suckled_ever, suckling_ever, n_photos) %>%
  filter(n_photos > 2 & !is.na(R_sd))

dim(dat_HD)

dat_HF <- clean_data %>%
  select(ID, Length = mean_TL, Length_SD = sd_TL,
         Ratio = mean_ratio.HF, R_sd = sd_ratio.HF,
         suckled_ever, suckling_ever, n_photos) %>%
  filter(n_photos > 2 & !is.na(R_sd))
dim(dat_HF)



#check:
#summarize raw CVs:



# 3. HD model - observed ----

#growth curves
hd_mod <-optim_sex(data = dat_HD, 
                   pard0 = c(fr = nish[3,2], fmax = nish[1,2], mr = nish[4,2], mmax = nish[2,2]), 
                   chm = 6)

hd_params <- hd_mod$params

#posterior probabilities
dat_HD$P_fem<-f_probs(params = hd_params, data = dat_HD)


# 4. HF model - observed ----

#growth curves
hf_mod <-optim_sex(data = dat_HF, 
                   pard0 = c(fr = nish[3,2], fmax = nish[1,2], mr = nish[4,2], mmax = nish[2,2]), 
                   chm = 6)

hf_params <- hf_mod$params
#posterior probabilities
dat_HF$P_fem<-f_probs(params = hf_params, data = dat_HF)

# 4. Bootstraps ------
library(boot)

#~~~~a. known error parameter for DJI Mini drone----
err_mean <- 0.0012 #observed error distribution
err_sd <- 0.0315

#~~~~b. setup the hybrid bootstrap function ----
foo <- function(data,indices){
  #non-parametric bit: inter-image variability
  dt <- data[indices,]
  
  #parametric bit: altitude estimation induced error
  #dt$Length <- dt$Length + rnorm(nrow(dt), mean = err_mean, sd = err_sd)
  
  #fit the model with bootstrap simulation:
  mod <-optim_sex(data = dt, 
                     pard0 = c(fr = nish[3,2], fmax = nish[1,2], mr = nish[4,2], mmax = nish[2,2]), 
                     chm = 6)
  params <- mod$params
  
  #estimate individual post probs:
  f_probs(params = params, data = dt)
  
}



#~~~~c. run bootstrap estimates for HD----
my_boot_HD <- boot(
  data = dat_HD, 
  statistic = foo, 
  R = 100
)

prob_boot <- my_boot_HD$t

hist(colMeans(prob_boot))

mean(prob_boot[,1])

median(prob_boot[,1])
hdi(prob_boot[,1], credMass = 0.95)


hist(dat_HD$P_fem)

plot(dat_HD$P_fem, my_boot_HD$t0)





boot.ci(my_boot_HD, index = 4)



plot(my_boot_HD,index = 1)



#~~~~c. run bootstrap estimates for HD----
my_boot_HF <- boot(
  data = dat_HF, 
  statistic = foo, 
  R = 10
)

boot_probs <- my_boot_HF$t
my_boot$data




plot(dat_HF$P_fem, my_boot_HF$t0)


plot(my_boot_HF, index = 4)


