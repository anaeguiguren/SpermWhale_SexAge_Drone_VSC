# Growth Curve Parameter Optimization
# Load necessary libraries and functions

rm(list = ls())

source("Scripts/functions.R")

set.seed(1234567)

# 1. Load data ----
#only rows that could be assigned individual IDs
clean_data <- read.csv("Data/Processed_Data/id_unpooled_clean_processed.csv") 
nish <- read.csv("Data/Processed_Data/nishiwaki_parameters.csv")

# subset relevant columns
dat<- clean_data %>%
  select(ID, Length = TL.m, 
         R.HD = ratio.HD, R.HF = ratio.HF, 
         suckled_ever, suckling_ever)





# make a data frame for HF ratio:
dat_HF <- dat %>%
  group_by(ID) %>%
  filter(sum(!is.na(R.HF)) >= 3)  %>%
  ungroup() %>%
  filter(!is.na(R.HF))

summary_data<-dat_HF %>% 
  group_by(ID)%>%
  summarize(n_photos = n())

cat("Number Identified whals:", length(levels(as.factor(dat$ID))), "\n")

#how many individuals
cat("Number Identified whals:", length(levels(as.factor(dat_HF$ID))), "\n")


# 2. Bootstrap Individual whales -----
n_boots <- 1000 # number of simulations


#initialize lists to hold loop values (for exponential and linear versions of the male curve)

boot_params_hd <-vector("list", n_boots) # parameters for HD ratio
boot_params_hf <-vector("list", n_boots) # parameters for HF ratio

#sum of squares

ss_hd <- vector("list", n_boots) 
ss_hf <- vector("list", n_boots)

#individual estimates of p_fem
dat_boot <- vector("list", n_boots)



for(i in 1:n_boots){
  
  # each time

  tmp.dat <- dat_HF %>%
    group_by(ID) %>%
    slice_sample(n = 1)
  
  hd.temp <- optim_sex(data = tmp.dat %>% mutate(Ratio = R.HD),
                      chm = 6, 
                      pard0 = c(fmax = nish$Value[1], 
                                fr = nish$Value[3], 
                                mmax = nish$Value[2], 
                                mr = nish$Value[4]), 
                      exponential_male_growth = TRUE,
                      weighted = FALSE)
  
  
  hf.temp <- optim_sex(tmp.dat %>% mutate(Ratio = R.HF),
                       chm = 6, 
                       pard0 =  c(fmax = nish$Value[1], 
                                  fr = nish$Value[3], 
                                  mmax = nish$Value[2], 
                                  mr = nish$Value[4]), 
                       exponential_male_growth = TRUE,
                       weighted = FALSE)
  

  
  #exponential
  tmp.dat$fem_prob_hd <- f_probs(params = hd.temp$params, 
                                 data = tmp.dat %>% mutate(Ratio = R.HD),
                                 chm = 6, 
                                 exponential_male_growth = TRUE, 
                                 weighted = FALSE)
  
  tmp.dat$fem_prob_hf <- f_probs(params = hf.temp$params, 
                                 data = tmp.dat %>% mutate(Ratio = R.HF),
                                 chm = 6, 
                                 exponential_male_growth = TRUE, 
                                 weighted = FALSE)
  
  
  tmp.dat$m_prob_hd <- m_probs(params = hd.temp$params, 
                               data = tmp.dat %>% mutate(Ratio = R.HD), 
                               chm = 6, 
                               exponential_male_growth = TRUE, 
                               weighted = FALSE)
  
  tmp.dat$m_prob_hf <- m_probs(params = hf.temp$params, 
                               data = tmp.dat %>% mutate(Ratio = R.HF),
                               chm = 6, 
                               exponential_male_growth = TRUE, 
                               weighted = FALSE)
  
  
  
  #save:
  
  
  boot_params_hd[[i]] <- hd.temp$params
  boot_params_hf[[i]] <- hf.temp$params
  
  ss_hd[[i]] <-hd.temp$ss
  ss_hf[[i]] <-hf.temp$ss
  
  dat_boot[[i]] <- tmp.dat
  
  
}


# 3. Get average and CI estimates-----

#individual measurements:
all_boot <- bind_rows(dat_boot, .id = "bootstrap")



boot_summary<-all_boot %>%
  group_by(ID) %>%
  summarize(
    mean_length = mean(Length, na.rm = TRUE),
    length_CI_low = quantile(Length, 0.025, na.rm = TRUE), 
    length_CI_hi = quantile(Length, 0.975, na.rm = TRUE,),
    sd_length = sd(Length, na.rm = T),
    
    mean_R.HD = mean(R.HD, na.rm = TRUE),
    sd_R.HD = sd(R.HD, na.rm = T),
    R.HD_CI_low = quantile(R.HD, 0.025, na.rm = TRUE), 
    R.HD_CI_hi = quantile(R.HD, 0.975, na.rm = TRUE),
    
    mean_R.HF = mean(R.HF, na.rm = TRUE),
    sd_R.HF = sd(R.HF, na.rm =  TRUE),
    R.HF_CI_low = quantile(R.HF, 0.025, na.rm = TRUE), 
    R.HF_CI_hi = quantile(R.HF, 0.975, na.rm = TRUE),
    
    mean_fem_prob_hd = mean(fem_prob_hd, na.rm = TRUE), 
    sd_fem_prob_hd = sd(fem_prob_hd, na.rm = T),
    prob_hd_CI_low = unname(quantile(fem_prob_hd, 0.025, na.rm = TRUE)),
    prob_hd_CI_hi = unname(quantile(fem_prob_hd, 0.975, na.rm = TRUE)),
    
    mean_m_prob_hd = mean(m_prob_hd, na.rm = TRUE), 
    m_prob_hd_CI_low = unname(quantile(m_prob_hd, 0.025, na.rm = TRUE)),
    m_prob_hd_CI_hi = unname(quantile(m_prob_hd, 0.975, na.rm = TRUE)),
    
    
    mean_fem_prob_hf = mean(fem_prob_hf, na.rm = TRUE), 
    sd_fem_prob_hf = sd(fem_prob_hf, na.rm = T),
    prob_hf_CI_low = unname(quantile(fem_prob_hf, 0.025, na.rm = TRUE)),
    prob_hf_CI_hi = unname(quantile(fem_prob_hf, 0.975, na.rm = TRUE)),
    
    mean_m_prob_hf = mean(m_prob_hf, na.rm = TRUE), 
    m_prob_hf_CI_low = unname(quantile(m_prob_hf, 0.025, na.rm = TRUE)),
    m_prob_hf_CI_hi = unname(quantile(m_prob_hf, 0.975, na.rm = TRUE)),
    
    suckled_ever = first(suckled_ever), 
    suckling_ever = first(suckling_ever)
  )



#. population parameters:

# extract bootstrapped parameters:
hd_params_df <- do.call(rbind, boot_params_hd)
hd_params_df <- as.data.frame(hd_params_df)

names(hd_params_df) <- c("fr", "fmax", "mr", "mmax")
summary(hd_params_df)


hf_params_df <- do.call(rbind, boot_params_hf)
hf_params_df <- as.data.frame(hf_params_df)

names(hf_params_df) <- c("fr", "fmax", "mr", "mmax")
summary(hf_params_df)



#compute confidence interval width for (p(f)) and p(m)
#female
boot_summary$CI_width_HD <- boot_summary$prob_hd_CI_hi - boot_summary$prob_hd_CI_low
boot_summary$CI_width_HF <- boot_summary$prob_hf_CI_hi - boot_summary$prob_hf_CI_low

boot_summary$CI_width_R.HD <- unname(boot_summary$R.HD_CI_hi) - unname(boot_summary$R.HD_CI_low)
boot_summary$CI_width_R.HF <- unname(boot_summary$R.HF_CI_hi) - unname(boot_summary$R.HF_CI_low)

#male
boot_summary$CI_width_HD_male <- boot_summary$m_prob_hd_CI_hi - boot_summary$m_prob_hd_CI_low
boot_summary$CI_width_HF_male <- boot_summary$m_prob_hf_CI_hi - boot_summary$m_prob_hf_CI_low
#they are equivalent -leave them be!

#CV for length,hf, hd

boot_summary$CV_length <- (boot_summary$sd_length/boot_summary$mean_length)*100
boot_summary$CV_HD <- (boot_summary$sd_R.HD/boot_summary$mean_R.HD)*100
boot_summary$CV_HF <- (boot_summary$sd_R.HF/boot_summary$mean_R.HF)*100


save.image(file = "bootstrapped_estimates.RData")
#load("bootstrapped_estimates.RData")

