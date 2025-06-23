# Growth Curve Parameter Optimization
# Load necessary libraries and functions
source("Scripts/functions.R")
library(boot)
set.seed(1234567)

# 1. Load data ----
#only rows that could be assigned individual IDs
clean_data <- read.csv("Data/Processed_Data/id_unpooled_clean_processed.csv") 
nish <- read.csv("Data/nishiwaki_parameters.csv")
# subset relevant columns
dat<- clean_data %>%
  select(ID, Length = TL.m, 
         R.HD = ratio.HD, R.HF = ratio.HF, 
         suckled_ever, suckling_ever)



# make a data frame for HD ratio:
#keep only rows from  individuals with >=3 ratio measurements available
dat_HD <- dat %>%
  group_by(ID) %>%
  filter(sum(!is.na(R.HD)) >= 3)  %>%
  ungroup() %>%
  filter(!is.na(R.HD))

# make a data frame for HF ratio:
dat_HF <- dat %>%
  group_by(ID) %>%
  filter(sum(!is.na(R.HF)) >= 3)  %>%
  ungroup() %>%
  filter(!is.na(R.HF))



# 2. Bootstrap Individual whales -----
n_boots <- 1000 # number of simulations


#initialize lists to hold loop values

boot_params_hd <-vector("list", n_boots) # parameters for HD ratio
boot_params_hf <-vector("list", n_boots) # parameters for HF ratio

ss_hd <- vector("list", n_boots) #sum of squares
ss_hf <- vector("list", n_boots)


dat_boot <- vector("list", n_boots) #individual estimates of p_fem


for(i in 1:n_boots){
  
  # each time

  tmp.dat <- dat_HF %>%
    group_by(ID) %>%
    slice_sample(n = 1)
  
  
  hd.temp <- optim_sex(tmp.dat %>% mutate(Ratio = R.HD),
                      chm = 6, 
                      pard0 = c(nish$Value[3],
                                nish$Value[1],
                                nish$Value[4],
                                nish$Value[2]), weighted = FALSE)
  
  hf.temp <- optim_sex(tmp.dat %>% mutate(Ratio = R.HF),
                       chm = 6, 
                       pard0 =  c(nish$Value[3],
                                  nish$Value[1],
                                  nish$Value[4],
                                  nish$Value[2]), weighted = FALSE)
  
  tmp.dat$fem_prob_hd <- f_probs(hd.temp$params, data = tmp.dat %>% mutate(Ratio = R.HD))
  
  tmp.dat$fem_prob_hf <- f_probs(hf.temp$params, data = tmp.dat %>% mutate(Ratio = R.HF))
  
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
    length_CI_hi = quantile(Length, 0.975, na.rm = TRUE),
    
    mean_R.HD = mean(R.HD, na.rm = TRUE),
    R.HD_CI_low = quantile(R.HD, 0.025, na.rm = TRUE), 
    R.HD_CI_hi = quantile(R.HD, 0.975, na.rm = TRUE),
    
    mean_R.HF = mean(R.HF, na.rm = TRUE),
    R.HF_CI_low = quantile(R.HF, 0.025, na.rm = TRUE), 
    R.HF_CI_hi = quantile(R.HF, 0.975, na.rm = TRUE),
    
    mean_fem_prob_hd = mean(fem_prob_hd, na.rm = TRUE), 
    prob_hd_CI_low = unname(quantile(fem_prob_hd, 0.025, na.rm = TRUE)),
    prob_hd_CI_hi = unname(quantile(fem_prob_hd, 0.975, na.rm = TRUE)),
    
    mean_fem_prob_hf = mean(fem_prob_hf, na.rm = TRUE), 
    prob_hf_CI_low = unname(quantile(fem_prob_hf, 0.025, na.rm = TRUE)),
    prob_hf_CI_hi = unname(quantile(fem_prob_hf, 0.975, na.rm = TRUE)),
    
    suckled_ever = first(suckled_ever), 
    suckling_ever = first(suckling_ever)
  )



#. population parameters:


ss_hd

bind_rows(boot_params_hd)

# 4. Visualize -----
#
boot_summary$CI_width_HD <- boot_summary$prob_hd_CI_hi - boot_summary$prob_hd_CI_low
boot_summary$CI_width_HF <- boot_summary$prob_hf_CI_hi - boot_summary$prob_hf_CI_low


ggplot(boot_summary, aes(x = mean_length, y = mean_R.HD, colour = mean_fem_prob_hd))+
  geom_point(aes(shape = suckled_ever, size = CI_width_HD), alpha = 0.9)+
  
  

ggplot(boot_summary, aes(x = mean_length, y = mean_R.HF, colour = mean_fem_prob_hf))+
  geom_point(aes(shape = suckled_ever, size = CI_width_HF), alpha = 0.9)



