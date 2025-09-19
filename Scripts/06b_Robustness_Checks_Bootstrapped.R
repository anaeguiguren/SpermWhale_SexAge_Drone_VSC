#Bootstrapped Robustness checks
# Check effects of varying chm and prior p(f) on posterior p(f) estimates
source("Scripts/functions.R")
library(patchwork)
library(wacolors)
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




# make a data frame for HF ratio:
dat_HF <- dat %>%
  group_by(ID) %>%
  filter(sum(!is.na(R.HF)) >= 3)  %>%
  ungroup() %>%
  filter(!is.na(R.HF))


# 2. Effect of varying chm  -----

#realistic range for chm (+-1 meter)
chm_range <- c(5,5.5,6, 6.5, 7)

n_sim <- length(chm_range) # number of simulations

n_boots <- 1000 # number of simulations


#initialize lists to hold loop values

boot_params_hf <-vector("list", n_boots) # parameters for HF ratio


dat_boot <- list()
counter <- 1

for (i in 1:n_boots) {
  
  # bootstrap sample
  tmp.dat <- dat_HF %>%
    group_by(ID) %>%
    slice_sample(n = 1)
  
  for (j in 1:n_sim) {
    hf.temp <- optim_sex(
      tmp.dat %>% mutate(Ratio = R.HF),
      chm = chm_range[j], 
      pard0 = c(nish$Value[3],
                nish$Value[1],
                nish$Value[4],
                nish$Value[2]), 
      weighted = FALSE
    )
    
    tmp.dat$fem_prob_hf <- f_probs(hf.temp$params, 
                                   data = tmp.dat %>% mutate(Ratio = R.HF))
    tmp.dat$chm <- chm_range[j]
    
    dat_boot[[counter]] <- tmp.dat
    counter <- counter + 1
  }
}

  
#~~~c. summarize by ID and chm ----  

all_boot <- bind_rows(dat_boot, .id = "bootstrap")

boot_summary<-all_boot %>%
  group_by(ID, chm) %>%
  summarize(
    mean_length = mean(Length, na.rm = TRUE),
    length_CI_low = quantile(Length, 0.025, na.rm = TRUE), 
    length_CI_hi = quantile(Length, 0.975, na.rm = TRUE),
    
    
    mean_R.HF = mean(R.HF, na.rm = TRUE),
    R.HF_CI_low = quantile(R.HF, 0.025, na.rm = TRUE), 
    R.HF_CI_hi = quantile(R.HF, 0.975, na.rm = TRUE),
    
    
    mean_fem_prob_hf = mean(fem_prob_hf, na.rm = TRUE), 
    prob_hf_CI_low = unname(quantile(fem_prob_hf, 0.025, na.rm = TRUE)),
    prob_hf_CI_hi = unname(quantile(fem_prob_hf, 0.975, na.rm = TRUE)),
    
    suckled_ever = first(suckled_ever), 
    suckling_ever = first(suckling_ever)
  )



boot_summary<- boot_summary %>%
  mutate(ID_short = substr(ID, 10, 11))

 
#~~~~d. make plots -----
p2<- boot_summary %>%
  ggplot(aes(x=reorder(ID_short, mean_fem_prob_hf), y = mean_fem_prob_hf))+
  geom_boxplot()+
  geom_jitter(aes(colour = chm),width = 0.2, alpha = 0.7)+
  labs(x = "ID",
       y = "P(f)",      
       colour = "chm (m)")+
  theme_classic()+
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust = 1
  ))

p2


ggsave("Figures/appendix_effect_of_chm_on_fprob_boxplots_boootstrapped.png",
       p2, width = 7, height = 4, dpi = 300)



# 2. Effect of varying prior p(f)  -----

#create new function that allows for priors to be modified:

f_probs_informed  <- function(params, data, prior_f = 0.5, chm = 6) {
  prior_m <- 1 - prior_f
  res <- sumsq(params, data, chm)
  likes <- exp(-res$likes / (2 * res$ss / (nrow(res$likes) - 1)))
  post_probs <- (likes[, 1] * prior_f) / (likes[, 1]* prior_f + likes[,2] * prior_m)
  return(post_probs)
}




n_boots <- 1000 # number of simulations


#initialize lists to hold loop values

n_boots <- 1000

dat_boot_2 <- list()
counter <- 1

for (i in 1:n_boots) {
  
  # bootstrap sample
  tmp.dat <- dat_HF %>%
    group_by(ID) %>%
    slice_sample(n = 1)
  
    hf.temp <- optim_sex(
      tmp.dat %>% mutate(Ratio = R.HF),
      chm = 6, 
      pard0 = c(nish$Value[3],
                nish$Value[1],
                nish$Value[4],
                nish$Value[2]), 
      weighted = FALSE
    )
    
    tmp.dat$fem_prob_hf <- f_probs_informed(hf.temp$params, 
                                            data = tmp.dat %>% mutate(Ratio = R.HF),
                                            prior_f = 0.5)
    
    tmp.dat$fem_prob_hf_inf <- f_probs_informed(hf.temp$params,
                                        data = tmp.dat %>% mutate(Ratio = R.HF),
                                        prior_f = 0.79)
    

    dat_boot_2[[counter]] <- tmp.dat
    counter <- counter + 1
}



#~~~c. summarize by ID  ----  

all_boot_2 <- bind_rows(dat_boot_2, .id = "bootstrap")

boot_summary_2<-all_boot_2 %>%
  group_by(ID) %>%
  summarize(
    mean_length = mean(Length, na.rm = TRUE),
    length_CI_low = quantile(Length, 0.025, na.rm = TRUE), 
    length_CI_hi = quantile(Length, 0.975, na.rm = TRUE),
    
    
    mean_R.HF = mean(R.HF, na.rm = TRUE),
    R.HF_CI_low = quantile(R.HF, 0.025, na.rm = TRUE), 
    R.HF_CI_hi = quantile(R.HF, 0.975, na.rm = TRUE),
    
    
    mean_fem_prob_hf = mean(fem_prob_hf, na.rm = TRUE), 
    prob_hf_CI_low = unname(quantile(fem_prob_hf, 0.025, na.rm = TRUE)),
    prob_hf_CI_hi = unname(quantile(fem_prob_hf, 0.975, na.rm = TRUE)),
    
    mean_fem_prob_hf_inf = mean(fem_prob_hf_inf, na.rm = TRUE), 
    prob_hf_CI_low_inf = unname(quantile(fem_prob_hf_inf, 0.025, na.rm = TRUE)),
    prob_hf_CI_hi_inf = unname(quantile(fem_prob_hf_inf, 0.975, na.rm = TRUE)),
    
    suckled_ever = first(suckled_ever), 
    suckling_ever = first(suckling_ever)
  )

boot_summary_2 %>%
  filter(ID == "GAL2023_011")

boot_summary_2<- boot_summary_2 %>%
  mutate(ID_short = substr(ID, 10, 11))



boot_summary_2$diff_fem_prob_hf <-boot_summary_2$mean_fem_prob_hf_inf -boot_summary_2$mean_fem_prob_hf
hist(boot_summary_2$diff_fem_prob_hf, breaks = 20)
#~~~~d. visualize results----

#convert to long format
df_long <- boot_summary_2 %>%
  pivot_longer(
    cols = c(mean_fem_prob_hf, mean_fem_prob_hf_inf), 
    names_to = "estimate_type",
    values_to = "estimate"
  )


# create an ordering variable only for the chosen estimate_type
order_ids <- df_long %>%
 filter(estimate_type == "mean_fem_prob_hf") %>%
  arrange(estimate) %>%
  pull(ID_short)

# make ID_short a factor with that order
df_long <- df_long %>%
  mutate(ID_short = factor(ID_short, levels = order_ids))

#make plot

p4 <- ggplot(df_long, aes(x = ID_short, y = estimate, colour = estimate_type))+
  geom_point(size = 2, alpha = 0.5)+
  geom_line(aes(group = ID),color="grey",lty = 2)+
  theme_classic()+
  labs(x = "ID",
       y = "P(f)",      
       colour = "Prior P(f)")+
  theme_classic(base_size = 8)+
  scale_colour_manual(values = c("black", "gray50"),
                      labels = c("P(f) = 0.5", "P(f) = 0.79"))+
  theme(axis.text.x = element_text(
    angle = 90, vjust = 1, hjust = 1
  ))

p4


ggsave("Figures/appendix_effect_prior_pf_bootstrapped.png",
       p4, width = 7, height = 4, dpi = 300)


df_long %>%
  filter(ID_short=="54")%>%
  select(estimate)
