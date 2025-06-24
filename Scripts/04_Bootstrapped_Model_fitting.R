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



# make a data frame for HD ratio:


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
  
  
  tmp.dat$m_prob_hd <- m_probs(hd.temp$params, data = tmp.dat %>% mutate(Ratio = R.HD))
  
  tmp.dat$m_prob_hf <- m_probs(hf.temp$params, data = tmp.dat %>% mutate(Ratio = R.HF))
  
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
    
    mean_m_prob_hd = mean(m_prob_hd, na.rm = TRUE), 
    m_prob_hd_CI_low = unname(quantile(m_prob_hd, 0.025, na.rm = TRUE)),
    m_prob_hd_CI_hi = unname(quantile(m_prob_hd, 0.975, na.rm = TRUE)),
    
    
    mean_fem_prob_hf = mean(fem_prob_hf, na.rm = TRUE), 
    prob_hf_CI_low = unname(quantile(fem_prob_hf, 0.025, na.rm = TRUE)),
    prob_hf_CI_hi = unname(quantile(fem_prob_hf, 0.975, na.rm = TRUE)),
    
    mean_m_prob_hf = mean(m_prob_hf, na.rm = TRUE), 
    m_prob_hf_CI_low = unname(quantile(m_prob_hf, 0.025, na.rm = TRUE)),
    m_prob_hf_CI_hi = unname(quantile(m_prob_hf, 0.975, na.rm = TRUE)),
    
    suckled_ever = first(suckled_ever), 
    suckling_ever = first(suckling_ever)
  )



#. population parameters:


# 4. Visualize -----

#compute confidence interval width for (p(f)) and p(m)
#female
boot_summary$CI_width_HD <- boot_summary$prob_hd_CI_hi - boot_summary$prob_hd_CI_low
boot_summary$CI_width_HF <- boot_summary$prob_hf_CI_hi - boot_summary$prob_hf_CI_low

#male
boot_summary$CI_width_HD_male <- boot_summary$m_prob_hd_CI_hi - boot_summary$m_prob_hd_CI_low
boot_summary$CI_width_HF_male <- boot_summary$m_prob_hf_CI_hi - boot_summary$m_prob_hf_CI_low
#they are equivalent!

#set scale limits so that same legend can be applied to both plots:
# Color scales
color_min <- min(boot_summary$mean_fem_prob_hd, boot_summary$mean_fem_prob_hf, na.rm = TRUE)
color_max <- max(boot_summary$mean_fem_prob_hd, boot_summary$mean_fem_prob_hf, na.rm = TRUE)

# Size scales
size_min <- min(boot_summary$CI_width_HD, boot_summary$CI_width_HF, na.rm = TRUE)
size_max <- max(boot_summary$CI_width_HD, boot_summary$CI_width_HF, na.rm = TRUE)


p1 <- ggplot(boot_summary, aes(x = mean_length, y = mean_R.HD))+
  geom_point(aes(colour = mean_fem_prob_hd, size = CI_width_HD, 
                 shape = factor(suckled_ever), alpha = 1.5), alpha = 0.8)+
  scale_color_wa_c("stuart",  limits = c(color_min, color_max)) +
  scale_size(limits = c(size_min, size_max))+
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4))+
  theme_classic()+
  geom_vline(xintercept = 13.7, linetype = "dashed")+
  labs(title = "A",
    x = "Length (m)",
       y = "Ratio (rostrum - dorsal fin)",
       colour = "P(fem)",
       size = "95-CI width",
       shape = "Suckled")+
  theme(legend.position = "null")


p2<-ggplot(boot_summary, aes(x = mean_length, y = mean_R.HF))+
  geom_point(aes(colour = mean_fem_prob_hf, size = CI_width_HF, 
                 shape = factor(suckled_ever)), alpha = 0.9)+
  scale_color_wa_c("stuart", , limits = c(color_min, color_max)) +
  scale_size(limits = c(size_min, size_max))+
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4))+
  theme_classic()+
  geom_vline(xintercept = 13.7, linetype = "dashed")+
  labs(title = "B",
       x = "Length (m)",
                       y = "Ratio (rostrum - flipper base)",
                       colour = "P(fem)",
                       size = "95-CI width",
                       shape = "Suckled")
p2

comb <- p1 + p2
comb 
  
ggsave("Figures/bootstrap_post_prob_models.png",
       comb, width = 10, height = 4)

# 5. visualize parameter estimation variation (HD)----
# extract bootstrapped parameters:
hd_params_df <- do.call(rbind, boot_params_hd)
hd_params_df <- as.data.frame(hd_params_df)

names(hd_params_df) <- c("fr", "fmax", "mr", "mmax")


hd_params_hf <- do.call(rbind, boot_params_hf)
hd_params_hf <- as.data.frame(hd_params_hf)

names(hd_params_hf) <- c("fr", "fmax", "mr", "mmax")


# create lines for each bootstrap:
min.L <- 4 # length at birth
max.L.F <- 12 # max length females
max.L.M <- 17 #Max male length


#length sequences:
x_F <- seq(min.L, max.L.F, by = 0.2)
x_M <- seq(min.L, max.L.M, by = 0.2)


# mean line:



#loop through:
all_f_lines <- list()
all_m_lines <- list()

for(i in 1:nrow(hd_params_df)){
  
  y_F <- fem_curve(length = x_F,
                   fr = hd_params_df$fr[i],
                   fmax = hd_params_df$fmax[i])
  
  
  y_M <- mal_curve(length = x_M,
                   fr = hd_params_df$fr[i],
                   fmax = hd_params_df$fmax[i], 
                   mr = hd_params_df$mr[i], 
                   mmax = hd_params_df$mmax[i],
                   chm = 6)
  
  f_line_hd <- data.frame(Length = x_F, Bootstrap = i, Sex = "F", Ratio = y_F)
  m_line_hd <- data.frame(Length = x_M, Bootstrap = i, Sex = "M",Ratio = y_M)
  
  #store in big list
  all_f_lines[[i]] <- f_line_hd
  all_m_lines[[i]] <- m_line_hd

}


# squish lists into data frames:

f_lines_df <- do.call(rbind, all_f_lines)
m_lines_df <- do.call(rbind, all_m_lines)

# make single data frame
all_lines_df <- rbind(f_lines_df, m_lines_df)

# make a mean line
mean_y_F <-  fem_curve(length = x_F,
                       fr = mean(hd_params_df$fr),
                       fmax = mean(hd_params_df$fmax))

mean_y_M <- mal_curve(length = x_M,
                      fr = median(hd_params_df$fr),
                      fmax = median(hd_params_df$fmax), 
                      mr = median(hd_params_df$mr), 
                      mmax = median(hd_params_df$mmax),
                      chm = 6)

mean_f_line <- data.frame(Length = x_F, Ratio = mean_y_F, Sex = "F", Bootstrap = 1)
mean_m_line <- data.frame(Length = x_M, Ratio = mean_y_M, Sex = "M", Bootstrap = 1)

mean_lines_df <- rbind(mean_f_line, mean_m_line)

#plot



ggplot(all_lines_df, aes(x = Length, y = Ratio, 
                         group = interaction(Bootstrap, Sex),
                         colour = Sex))+
  geom_line(alpha = 0.1)+
  geom_line(data = mean_lines_df, aes(x = Length, y = Ratio, colour = "grey"), 
            linewidth = 1.2) +  # mean lines
  scale_color_manual(values = c("F" = "blue", "M" = "red"))+
  theme_classic()
  






# 5. visualize parameter estimation variation (HF)----
# extract bootstrapped parameters:
hf_params_df <- do.call(rbind, boot_params_hf)
hf_params_df <- as.data.frame(hf_params_df)

names(hf_params_df) <- c("fr", "fmax", "mr", "mmax")

#loop through:
all_f_lines <- list()
all_m_lines <- list()

for(i in 1:nrow(hf_params_df)){
  
  y_F <- fem_curve(length = x_F,
                   fr = hf_params_df$fr[i],
                   fmax = hf_params_df$fmax[i])
  
  
  y_M <- mal_curve(length = x_M,
                   fr = hf_params_df$fr[i],
                   fmax = hf_params_df$fmax[i], 
                   mr = hf_params_df$mr[i], 
                   mmax = hf_params_df$mmax[i],
                   chm = 6)
  
  f_line_hd <- data.frame(Length = x_F, Bootstrap = i, Sex = "F", Ratio = y_F)
  m_line_hd <- data.frame(Length = x_M, Bootstrap = i, Sex = "M",Ratio = y_M)
  
  #store in big list
  all_f_lines[[i]] <- f_line_hd
  all_m_lines[[i]] <- m_line_hd
  
}


# squish lists into data frames:

f_lines_df <- do.call(rbind, all_f_lines)
m_lines_df <- do.call(rbind, all_m_lines)

# make single data frame
all_lines_df <- rbind(f_lines_df, m_lines_df)

# make a mean line
mean_y_F <-  fem_curve(length = x_F,
                       fr = mean(hf_params_df$fr),
                       fmax = mean(hf_params_df$fmax))

mean_y_M <- mal_curve(length = x_M,
                      fr = mean(hf_params_df$fr),
                      fmax = mean(hf_params_df$fmax), 
                      mr = mean(hf_params_df$mr), 
                      mmax = mean(hf_params_df$mmax),
                      chm = 6)

mean_f_line <- data.frame(Length = x_F, Ratio = mean_y_F, Sex = "F", Bootstrap = 1)
mean_m_line <- data.frame(Length = x_M, Ratio = mean_y_M, Sex = "M", Bootstrap = 1)

mean_lines_df <- rbind(mean_f_line, mean_m_line)

#plot



ggplot(all_lines_df, aes(x = Length, y = Ratio, 
                         group = interaction(Bootstrap, Sex),
                         colour = Sex))+
  geom_line(alpha = 0.05)+
  geom_line(data = mean_lines_df, aes(x = Length, y = Ratio, colour = "grey"), 
            linewidth = 1.2) +  # mean lines
  scale_color_manual(values = c("F" = "blue", "M" = "red"))+
  theme_classic()



# 6. visualize individual - level variations----
boot_summary%>%
  ggplot(aes(x = mean_R.HD, y = mean_fem_prob_hd, colour = mean_fem_prob_hd))+
  scale_color_wa_c("stuart", , limits = c(color_min, color_max)) +
  geom_errorbar(aes(ymin = prob_hd_CI_low, ymax = prob_hd_CI_hi ))+
  geom_point(aes(shape = suckled_ever))+
  theme_classic()
  

boot_summary%>%
  ggplot(aes(x = mean_R.HF, y = mean_fem_prob_hf, colour = mean_fem_prob_hf))+
  scale_color_wa_c("stuart", , limits = c(color_min, color_max)) +
  geom_errorbar(aes(ymin = prob_hf_CI_low, ymax = prob_hf_CI_hi ))+
  geom_point(aes(shape = suckled_ever))+
  theme_classic()
