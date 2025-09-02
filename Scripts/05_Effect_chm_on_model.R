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



# 3. Get average and CI estimates-----

#individual measurements:
all_sim <- bind_rows(dat_sim, .id = "bootstrap")




ggplot(all_sim, aes(x= reorder(ID, Length), y = fem_prob_hf))+
  geom_point(aes(colour = chm), alpha = 0.5, size = 3)+
  theme(axis.text.x = element_text(angle=90, hjust =1))



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

