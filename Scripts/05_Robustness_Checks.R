#Visualize -----
rm(list = ls())

source("Scripts/functions.R")
library(wacolors)
library(patchwork)
library(ggrepel)

# 1. Load and prep Data -----
load("bootstrapped_estimates_plus_mean.RData")


dat<-dat %>% 
  mutate(short_ID = substr(ID, 10, 11), 
         pd_detected = ifelse(suckling_ever == TRUE, "doing",
                              ifelse(suckled_ever == TRUE, "receiving", "no")))


# 1. Load data ----
#only rows that could be assigned individual IDs
#clean_data <- read.csv("Data/Processed_Data/id_unpooled_clean_processed.csv") 
nish <- read.csv("Data/Processed_Data/nishiwaki_parameters.csv")







# 2. Effect of varying chm  -----

#realistic range for chm (+-1 meter)
chm_range <- c(5,5.5,6, 6.5, 7)

n_sim <- length(chm_range) # number of simulations

#initialize lists to hold loop values

params_hf <-vector("list", n_sim) # parameters for HF ratio
dat_chm <- list()
counter <- 1
  
for (j in 1:n_sim) {
  tmp.dat <- dat
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
    
    dat_chm[[counter]] <- tmp.dat
    counter <- counter + 1
  }


  
#~~~c. summarize by ID and chm ----  

dat_chm <- bind_rows(dat_chm)

#dat_chm_summ<-dat_chm %>%
 # group_by(ID) %>%
  #summarize(
   # mean_fem_prob_hf = mean(fem_prob_hf, na.rm = TRUE), 
  #  prob_hf_CI_low = unname(quantile(fem_prob_hf, 0.025, na.rm = TRUE)),
  #  prob_hf_CI_hi = unname(quantile(fem_prob_hf, 0.975, na.rm = TRUE)),
  #)



dat_chm<- dat_chm %>%
  mutate(ID_short = substr(ID, 10, 11))

 
#~~~~d. make plots -----
p2<- dat_chm %>%
  ggplot(aes(x=reorder(ID_short, fem_prob_hf), y = fem_prob_hf))+
  geom_boxplot()+
  geom_jitter(aes(colour = chm),width = 0.2, alpha = 0.7)+
  labs(x = "ID",
       y = "P(f)",      
       colour = "chm (m)")+
  theme_classic()+
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust = 1
  ))




ggsave("Figures/Final_Figures/Sup_FigS4_1_effect_of_chm_on_fprob_boxplots_boootstrapped.png",
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






#initialize lists to hold loop values


dat_p <- list()
counter <- 1


  
# bootstrap sample
# uninformed (pfem = 0.5)
hf.temp <- optim_sex(
      dat %>% mutate(Ratio = R.HF),
      chm = 6, 
      pard0 = c(nish$Value[3],
                nish$Value[1],
                nish$Value[4],
                nish$Value[2]), 
      weighted = FALSE
    )


#uninformed = p_fem prior = 0.5    
dat$fem_prob_hf_u <- f_probs_informed(hf.temp$params, 
                                            data = dat %>% mutate(Ratio = R.HF),
                                            prior_f = 0.5)
    
dat$fem_prob_hf_inf <- f_probs_informed(hf.temp$params,
                                        data = dat %>% mutate(Ratio = R.HF),
                                        prior_f = 0.79)


#should be zero  
hist(dat$P_fem_HF-dat$fem_prob_hf_u)#nice

dat$diff_fem_prob_hf <-dat$fem_prob_hf_inf -dat$fem_prob_hf_u
hist(dat$diff_fem_prob_hf, breaks = 20)
#~~~~d. visualize results----

#convert to long format
df_long <- dat %>%
  pivot_longer(
    cols = c(fem_prob_hf_u, fem_prob_hf_inf), 
    names_to = "estimate_type",
    values_to = "estimate"
  )


# create an ordering variable only for the chosen estimate_type
order_ids <- df_long %>%
 filter(estimate_type == "fem_prob_hf_u") %>%
  arrange(estimate) %>%
  pull(short_ID)

# make ID_short a factor with that order
df_long <- df_long %>%
  mutate(short_ID = factor(short_ID, levels = order_ids))

#make plot

p4 <- ggplot(df_long, aes(x = short_ID, y = estimate, colour = estimate_type))+
  geom_point(size = 2, alpha = 0.5)+
  geom_line(aes(group = ID),color="grey",lty = 2)+
  theme_classic()+
  labs(x = "ID",
       y = "P(f)",      
       colour = "Prior P(f)")+
  theme_classic(base_size = 8)+
  scale_colour_manual(values = c("gray50", "black"),
                   labels = c("P(f) = 0.79", "P(f) = 0.5"))+
  theme(axis.text.x = element_text(
    angle = 90, vjust = 1, hjust = 1
  ))

p4


ggsave("Figures/Final_Figures/Sup_FigS4_2_effect_prior_pf.png",
       p4, width = 7, height = 4, dpi = 300)


df_long %>%
  filter(short_ID=="11")%>%
  select(estimate)

df_long %>%
  filter(Length < 5)%>%
  select(estimate)
