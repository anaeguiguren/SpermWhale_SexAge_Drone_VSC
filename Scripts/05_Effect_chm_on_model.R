#04_Bootstrapped_HF_Model_Fitting
# Growth Curve Parameter Optimization
# Load necessary libraries and functions
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

# get a mean data frame by individual 

dat_HF_mean <- dat_HF%>%
  group_by(ID)%>%
  summarize(ID = first(ID), Length = mean(Length), 
            R.HD = mean(R.HD), R.HF = mean(R.HF), 
            suckled_ever = first(suckled_ever), 
            suckling_ever = first(suckling_ever))



# 2. Run model with different chm values -----
chm_range <- seq(5, 9, length.out =50)


n_sim <- length(chm_range) # number of simulations


#initialize lists to hold loop values

sim_params_hf <-vector("list", n_sim) # parameters for HF ratio

ss_hf <- vector("list", n_sim)


dat_sim <- vector("list", n_sim) #individual estimates of p_fem




for(i in 1:n_sim){
  
  # each time
  
  tmp.dat <-dat_HF_mean

  
  hf.temp <- optim_sex(tmp.dat %>% mutate(Ratio = R.HF),
                       chm = chm_range[i], 
                       pard0 =  c(nish$Value[3],
                                  nish$Value[1],
                                  nish$Value[4],
                                  nish$Value[2]), weighted = FALSE)
  

  tmp.dat$fem_prob_hf <- f_probs(hf.temp$params, data = tmp.dat %>% mutate(Ratio = R.HF))
  tmp.dat$chm <-chm_range[i]
  
 
  
  #save:
  
  
  sim_params_hf[[i]] <- hf.temp$params
  
  ss_hf[[i]] <-hf.temp$ss
  
  dat_sim[[i]] <- tmp.dat
  
  
}




#individual measurements:
all_sim <- bind_rows(dat_sim, .id = "sim")



p1<-ggplot(all_sim, aes(x= chm, y = fem_prob_hf, group = ID))+
  geom_line(aes(colour = Length), alpha = 0.8)+
  scale_colour_viridis_c() + 
  labs(x = "Divergence length (chm - m)",
       y = "P(f)",      
       colour = "Length (m)")+
  geom_vline(xintercept  = 6, colour = "gray",lty =3)+
  theme_classic()

p1


ggsave("Figures/appendix_effect_of_chm_on_fprob.png",
       p1, width = 6, height = 3)

#a plot showing individual variation in chm between 5 - 7 m

p2 <- all_sim %>%
  filter(chm<=7) %>%
  ggplot(aes(x=reorder(ID, fem_prob_hf), y = fem_prob_hf))+
  geom_boxplot()+
  geom_jitter(aes(colour = chm),width = 0.1, alpha = 0.5)+
  labs(x = "ID",
       y = "P(f)",      
       colour = "chm (m)")+
  theme_classic()+
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust = 1
  ))

p2


ggsave("Figures/appendix_effect_of_chm_on_fprob_boxplots.png",
       p2, width = 9, height = 5)



# get boot_summary data

load("bootstrapped_estimates.RData")
head(all_sim)


chm_sim_summary<-all_sim%>%
  filter(chm<=7)%>%
  group_by(ID)%>%
  summarize(femp_prob_range = max(fem_prob_hf)- min(fem_prob_hf))



chm_sim_summary<-left_join(chm_sim_summary, boot_summary, by = "ID")

chm_sim_summary <- chm_sim_summary %>%
  mutate(CI_width = prob_hf_CI_hi- prob_hf_CI_low, 
         high_range = ifelse(femp_prob_range>0.05, "high", "low"))


chm_sim_summary<-chm_sim_summary%>%
  mutate(short_ID = substr(ID, 10, 11), 
       label_show =
         ifelse(femp_prob_range>0.05,
                short_ID, 
                ""))


#make subset of data with values highe
color_min <- min(boot_summary$mean_fem_prob_hd, boot_summary$mean_fem_prob_hf, na.rm = TRUE)
color_max <- max(boot_summary$mean_fem_prob_hd, boot_summary$mean_fem_prob_hf, na.rm = TRUE)


p3<-ggplot(chm_sim_summary, aes(x = femp_prob_range, y = CI_width, colour = mean_fem_prob_hf))+
  geom_point(alpha = 0.7, size = 3)+
  geom_text(aes(label = label_show), 
            hjust = 0, nudge_x = 0.005)+
  geom_vline(xintercept = 0.05, lty = 2, col = "gray")+
  scale_color_wa_c("puget", limits = c(color_min, color_max), reverse = T) +
  theme_classic()+
  labs(x = "P(f) range accross chm values",
       y = "P(f) bootsrapped 95% CI widths",      
       colour = "P(f)")+
  theme_classic()




ggsave("Figures/appendix_effect_of_chm_on_fprob_ciwidth.png",
       p3, width = 5, height = 4)

# 2. Run model with different prior pf values -----




