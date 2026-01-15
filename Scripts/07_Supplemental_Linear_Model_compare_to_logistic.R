# Compare and contrast linear version of model 

rm(list = ls())

source("Scripts/functions.R")
library(wacolors)
library(patchwork)
library(ggrepel)

# 1. Load and prep Data -----
load("bootstrapped_estimates_linear_version.RData")


# 2. Compare Sum of squares -----
#make a single data frame for plotting
l <- length(hd_ss_df$ss)

ss_df <- data.frame(
  ss = c(hd_ss_df$ss, hd_ss_df_lin$ss, hf_ss_df$ss, hf_ss_df_lin$ss), 
  NR_met = rep(c("HD", "HF"), each = l*2),# type of NR measure
  mod = rep(c("logistic", "linear"), each = l, times = 2)
)


# summarize

ss_sum<- ss_df %>%
  group_by(NR_met, mod) %>%
  summarize(
    mean = mean(ss),
    ss_CI_low = quantile(ss, 0.025, na.rm = TRUE), 
    ss_CI_hi = quantile(ss, 0.975, na.rm = TRUE,),
    percent_1 = mean(ss > 1, na.rm = TRUE),
  )

ss_sum <- ss_sum %>%
  mutate(grp = paste(NR_met, mod, sep = "_"))

ss_sum
#plot

p0 <-ggplot(ss_sum, aes(x = mod, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ss_CI_low, ymax = ss_CI_hi), width = 0) +
  geom_hline(yintercept = 1, lty = "dashed")+
  facet_wrap(~NR_met)+
  labs(
    y = "Sum of squares",
    x = "Model type",
    color = "model"
  ) +
  theme_classic()
p0

ggsave("Figures/Supplemental/Linear_v_logistic_SS.png",
       p0, width = 6, height =4 )


# ~~~~filtered 
#find out which bootstraps have ss>1
bad_ss_boots<- ss_df %>%
  mutate(Bootstrap = rep(seq(1,1000), 4))%>%
  filter(NR_met == "HD" & mod == "linear" & ss > 0.05)%>%
  select(Bootstrap)


# store original version with a new name
hd_params_df_lin_original <- hd_params_df_lin

#assign boot id's
hd_params_df_lin <- hd_params_df_lin %>%
  mutate(Bootstrap = seq(1,1000))

#retain only good ones (ss <- 1)
hd_params_df_lin <- hd_params_df_lin %>%
  filter(!Bootstrap  %in% bad_ss_boots$Bootstrap)




# 3. Visualize output curves -----

# create lines for each bootstrap:
min.L <- 4 # length at birth
max.L.F <- 16.5 # max length females
max.L.M <- 16.5 #Max male length


#length sequences:
x_F <- seq(min.L, max.L.F, by = 0.2)
x_M <- seq(6, max.L.M, by = 0.2)

#~~~~~~~i. HD-----
#loop through:
all_f_lines_hd <- list()
all_m_lines_hd <- list()

all_f_lines_hd_lin <- list()
all_m_lines_hd_lin <- list()



#logistic:

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
  all_f_lines_hd[[i]] <- f_line_hd
  all_m_lines_hd[[i]] <- m_line_hd
  
}


# linear

for(i in 1:nrow(hd_params_df_lin)){
  
  

  y_F_lin <- fem_curve(length = x_F,
                       fr = hd_params_df_lin$fr[i],
                       fmax = hd_params_df_lin$fmax[i])
  
  
  y_M_lin <- mal_curve_l(length = x_M,
                         fr = hd_params_df_lin$fr[i],
                         fmax = hd_params_df_lin$fmax[i], 
                         mr = hd_params_df_lin$mr_l[i], 
                         chm = 6)
  
  f_line_hd_lin <- data.frame(Length = x_F, Bootstrap = i, Sex = "F", Ratio = y_F_lin)
  m_line_hd_lin <- data.frame(Length = x_M, Bootstrap = i, Sex = "M",Ratio = y_M_lin)
  
  
  
  #store in big list
  all_f_lines_hd_lin[[i]] <- f_line_hd_lin
  all_m_lines_hd_lin[[i]] <- m_line_hd_lin
  
}

# squish lists into data frames:

f_lines_hd <- do.call(rbind, all_f_lines_hd)
m_lines_hd <- do.call(rbind, all_m_lines_hd)


f_lines_hd_lin <- do.call(rbind, all_f_lines_hd_lin)
m_lines_hd_lin <- do.call(rbind, all_m_lines_hd_lin)


# make single data frame
all_lines_hd <- rbind(f_lines_hd, m_lines_hd)

all_lines_hd_lin <- rbind(f_lines_hd_lin, m_lines_hd_lin)



summary(all_lines_hd)
summary(all_lines_hd_lin)
# make a mean line
# mean values at each interval

mean_curve <- function(df) {
  result <- by(df, df$Length, function(subset) {
    data.frame(
      Length = unique(subset$Length),
      
      #fr
      Ratio = mean(subset$Ratio, na.rm = T)
    )
  })
  
  do.call(rbind, result)
}

f_mean_line_hd <- mean_curve(f_lines_hd)
m_mean_line_hd <- mean_curve(m_lines_hd)


f_mean_line_hd_lin <- mean_curve(f_lines_hd_lin)
m_mean_line_hd_lin <- mean_curve(m_lines_hd_lin)


# store for plot

#~~~~logistic

mean_f_line_hd <- data.frame(Length = f_mean_line_hd$Length,
                             Ratio = f_mean_line_hd$Ratio, Sex = "Fem", Bootstrap = 1)

mean_m_line_hd <- data.frame(Length = m_mean_line_hd$Length,
                             Ratio = m_mean_line_hd$Ratio, Sex = "Mal", Bootstrap = 1)

mean_lines_hd <- rbind(mean_f_line_hd, mean_m_line_hd)

#~~~~linear


mean_f_line_hd_lin <- data.frame(Length = f_mean_line_hd_lin$Length,
                             Ratio = f_mean_line_hd_lin$Ratio, Sex = "Fem", Bootstrap = 1)

mean_m_line_hd_lin <- data.frame(Length = m_mean_line_hd_lin$Length,
                             Ratio = m_mean_line_hd_lin$Ratio, Sex = "Mal", Bootstrap = 1)

mean_lines_hd_lin <- rbind(mean_f_line_hd_lin, mean_m_line_hd_lin)

#~~~~~~~ii.HF-----
#loop through:
all_f_lines_hf <- list()
all_m_lines_hf <- list()


all_f_lines_hf_lin <- list()
all_m_lines_hf_lin <- list()

for(i in 1:nrow(hf_params_df)){
  
  # logistic
  y_F <- fem_curve(length = x_F,
                   fr = hf_params_df$fr[i],
                   fmax = hf_params_df$fmax[i])
  
  
  y_M <- mal_curve(length = x_M,
                   fr = hf_params_df$fr[i],
                   fmax = hf_params_df$fmax[i], 
                   mr = hf_params_df$mr[i], 
                   mmax = hf_params_df$mmax[i],
                   chm = 6)
  
  f_line_hf <- data.frame(Length = x_F, Bootstrap = i, Sex = "F", Ratio = y_F)
  m_line_hf <- data.frame(Length = x_M, Bootstrap = i, Sex = "M",Ratio = y_M)
  
  
  
  #linear
  y_F_lin <- fem_curve(length = x_F,
                   fr = hf_params_df_lin$fr[i],
                   fmax = hf_params_df_lin$fmax[i])
  
  
  y_M_lin <- mal_curve_l(length = x_M,
                   fr = hf_params_df_lin$fr[i],
                   fmax = hf_params_df_lin$fmax[i], 
                   mr_l = hf_params_df_lin$mr[i], 
                   chm = 6)
  
  f_line_hf_lin <- data.frame(Length = x_F, Bootstrap = i, Sex = "F", Ratio = y_F_lin)
  m_line_hf_lin <- data.frame(Length = x_M, Bootstrap = i, Sex = "M",Ratio = y_M_lin)
  
  
  #store in big list
  all_f_lines_hf[[i]] <- f_line_hf
  all_m_lines_hf[[i]] <- m_line_hf
  
  
  all_f_lines_hf_lin[[i]] <- f_line_hf_lin
  all_m_lines_hf_lin[[i]] <- m_line_hf_lin

  }


# squish lists into data frames:

#~~~~logistic
f_lines_hf <- do.call(rbind, all_f_lines_hf)
m_lines_hf <- do.call(rbind, all_m_lines_hf)

# make single data frame
all_lines_hf <- rbind(f_lines_hf, m_lines_hf)

summary(all_lines_hf)

#~~~~linear
f_lines_hf_lin <- do.call(rbind, all_f_lines_hf_lin)
m_lines_hf_lin <- do.call(rbind, all_m_lines_hf_lin)

# make single data frame
all_lines_hf_lin <- rbind(f_lines_hf_lin, m_lines_hf_lin)

summary(all_lines_hf_lin)
# make a mean line
# mean values at each interval

mean_curve <- function(df) {
  result <- by(df, df$Length, function(subset) {
    data.frame(
      Length = unique(subset$Length),
      
      #fr
      Ratio = mean(subset$Ratio, na.rm = T)
    )
  })
  
  do.call(rbind, result)
}

f_mean_line_hf <- mean_curve(f_lines_hf)
m_mean_line_hf <- mean_curve(m_lines_hf)


f_mean_line_hf_lin <- mean_curve(f_lines_hf_lin)
m_mean_line_hf_lin <- mean_curve(m_lines_hf_lin)



# store for plot

#~~~~logistic
mean_f_line_hf <- data.frame(Length = f_mean_line_hf$Length,
                             Ratio = f_mean_line_hf$Ratio, Sex = "Fem", Bootstrap = 1)

mean_m_line_hf <- data.frame(Length = m_mean_line_hf$Length,
                             Ratio = m_mean_line_hf$Ratio, Sex = "Mal", Bootstrap = 1)

mean_lines_hf <- rbind(mean_f_line_hf, mean_m_line_hf)


#~~~~linear


mean_f_line_hf_lin <- data.frame(Length = f_mean_line_hf_lin$Length,
                             Ratio = f_mean_line_hf_lin$Ratio, Sex = "Fem", Bootstrap = 1)

mean_m_line_hf_lin <- data.frame(Length = m_mean_line_hf_lin$Length,
                             Ratio = m_mean_line_hf_lin$Ratio, Sex = "Mal", Bootstrap = 1)

mean_lines_hf_lin <- rbind(mean_f_line_hf_lin, mean_m_line_hf_lin)




#~~~~~~~iii.plot----

#HD

#logistic
p3_log<-ggplot(all_lines_hd, aes(x = Length, y = Ratio, 
                             group = interaction(Bootstrap, Sex),
                             colour = Sex))+
  geom_vline(xintercept = 6, alpha = 0.7) +
  geom_line(alpha = 0.05, linewidth = 0.5)+
  geom_line(data = mean_f_line_hd, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth = 1, alpha = 1) +  # mean lines
  geom_line(data = mean_m_line_hd, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth =1, alpha = 1, linetype = "dashed") +  # mean lines
  scale_color_manual(values = c("F" = "darkcyan", "M" = "darkorange", 
                                "Fem" = "#b3dcdc", "Mal" = "#ffddb3"))+
  scale_y_continuous(limits = c(0.56, 0.75))+
  #geom_text(data = whaling_lables_hd, aes(x = Length+0.1,label = label),
  #          y =0.745,
  #          hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "Length (m)", y = expression(NR[dorsal]), title = "a)")+
  theme(legend.position = "null")


p3_log


#linear
p3_lin<-ggplot(all_lines_hd_lin, aes(x = Length, y = Ratio, 
                                 group = interaction(Bootstrap, Sex),
                                 colour = Sex))+
  geom_vline(xintercept = 6, alpha = 0.7) +
  geom_line(alpha = 0.05, linewidth = 0.5)+
  geom_line(data = mean_f_line_hd_lin, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth = 1, alpha = 1) +  # mean lines
  geom_line(data = mean_m_line_hd_lin, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth =1, alpha = 1, linetype = "dashed") +  # mean lines
  scale_color_manual(values = c("F" = "darkcyan", "M" = "darkorange", 
                                "Fem" = "#b3dcdc", "Mal" = "#ffddb3"))+
  scale_y_continuous(limits = c(0.56, 0.75))+
  #geom_text(data = whaling_lables_hd, aes(x = Length+0.1,label = label),
  #          y =0.745,
  #          hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "Length (m)", y = expression(NR[dorsal]), title = "b)")+
  theme(legend.position = "null")


p3_lin


#HF

p4_log<-ggplot(all_lines_hf, aes(x = Length, y = Ratio, 
                             group = interaction(Bootstrap, Sex),
                             colour = Sex))+
  geom_vline(xintercept = 6, alpha = 0.3)+
  geom_line(alpha = 0.05, linewidth = 0.5)+
  geom_line(data = mean_f_line_hf, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth = 1, alpha = 1) +  # mean lines
  geom_line(data = mean_m_line_hf, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth =1, alpha = 1 ,linetype = "dashed") +  # mean lines
  
  scale_color_manual(values = c("F" = "darkcyan", "M" = "darkorange", 
                                "Fem" = "#b3dcdc", "Mal" = "#ffddb3"))+
  # scale_y_continuous(limits = c(0.24, 0.42))+
  #geom_text(data = whaling_lables_hf, aes(x = Length+0.1, label = label),
  #          y = 0.425,
  #          hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "Length (m)", y = expression(NR[flipper]), title = "c)")+
  guides(colour = "none")


p4_log

p4_lin<-ggplot(all_lines_hf_lin, aes(x = Length, y = Ratio, 
                                 group = interaction(Bootstrap, Sex),
                                 colour = Sex))+
  geom_vline(xintercept = 6, alpha = 0.3)+
  geom_line(alpha = 0.05, linewidth = 0.5)+
  geom_line(data = mean_f_line_hf_lin, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth = 1, alpha = 1) +  # mean lines
  geom_line(data = mean_m_line_hf_lin, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth =1, alpha = 1 ,linetype = "dashed") +  # mean lines
  
  scale_color_manual(values = c("F" = "darkcyan", "M" = "darkorange", 
                                "Fem" = "#b3dcdc", "Mal" = "#ffddb3"))+
  scale_y_continuous(limits = c(0.24, 0.42))+
  #geom_text(data = whaling_lables_hf, aes(x = Length+0.1, label = label),
  #          y = 0.425,
  #          hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "Length (m)", y = expression(NR[flipper]), title = "d)")+
  guides(colour = "none")


p4_lin




curves <- (p3_log + p3_lin)/(p4_log + p4_lin)

curves
ggsave("Figures/Supplemental/Bootstrap_curves_lin_v_log.png",
       curves, width = 8, height = 8)

ggsave("Figures/Final_Figures/Fig5_bootstrap_params_curves_HF.png",
       p4_log+labs(title = ""), width = 7, height = 4)




# just plot the mean lines

#dorsal
p3_log_mean<-ggplot(mean_f_line_hd, aes(x = Length, y = Ratio))+
  scale_y_continuous(limits = c(0.56, 0.75))+
  geom_vline(xintercept = 6, alpha = 0.7) +
  geom_line(alpha = 1, linewidth = 1,colour = "darkcyan")+
  geom_line(data = mean_m_line_hd, 
            aes(x = Length, y = Ratio), linewidth = 1,
            colour = "darkorange")+
  geom_line(data = mean_f_line_hd_lin, 
            aes(x = Length, y = Ratio),
            alpha = 1, linewidth = 1,colour = "#b3dcdc", linetype = "dashed")+
  geom_line(data = mean_m_line_hd_lin, 
            aes(x = Length, y = Ratio),
            alpha = 1, linewidth = 1,colour = "#ffddb3", linetype = "dashed")+
  theme_classic()+
  labs(x = "Length (m)", y = expression(NR[dorsal]), title = "a)")
  

#flipper

p4_log_mean<-ggplot(mean_f_line_hf, aes(x = Length, y = Ratio))+
  scale_y_continuous(limits = c(0.24, 0.42))+
  geom_vline(xintercept = 6, alpha = 0.7) +
  geom_line(alpha = 1, linewidth = 1,colour = "darkcyan")+
  geom_line(data = mean_m_line_hf, 
            aes(x = Length, y = Ratio), linewidth = 1,
            colour = "darkorange")+
  geom_line(data = mean_f_line_hf_lin, 
            aes(x = Length, y = Ratio),
            alpha = 1, linewidth = 1,colour = "#b3dcdc", linetype = "dashed")+
  geom_line(data = mean_m_line_hf_lin, 
            aes(x = Length, y = Ratio),
            alpha = 1, linewidth = 1,colour = "#ffddb3", linetype = "dashed")+
  theme_classic()+
  labs(x = "Length (m)", y = expression(NR[flipper]), title = "b)")


mean_curves<-p3_log_mean + p4_log_mean

ggsave("Figures/Supplemental/mean_curves_lin_v_log.png",
       mean_curves, width = 8, height = 4)

 


# 4. Compare individual p(f) estimates and uncertainty-----
pf_hd<-ggplot(boot_summary, 
       aes(x = mean_fem_prob_hd, y = mean_fem_prob_hd_lin))+
  geom_point()+
  theme_classic()+
  xlim(0,1)+ylim(0,1)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "darkgray")+
  
  labs(x = "p(fem) - logistic", 
       y = "p(fem) - logistic/linear",
       title = "a)")

pf_hf<-ggplot(boot_summary, 
              aes(x = mean_fem_prob_hf,
                  y = mean_fem_prob_hf_lin))+
  geom_point()+
  theme_classic()+
  xlim(0,1)+ylim(0,1)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "darkgray")+
  
  labs(x = "p(fem) - logistic", 
       y = "",
       title = "b)")  


pf_plots<- pf_hd + pf_hf

ggsave("Figures/Supplemental/pfem_lin_v_log.png",
       pf_plots, width = 8, height = 4)


# compare uncertainty
cert_hd<-ggplot(boot_summary, 
              aes(x = CI_width_HD, y = CI_width_HD_lin))+
  geom_point()+
  theme_classic()+
  xlim(0,1)+ylim(0,1)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "darkgray")+
  
  labs(x = "95% CI width - logistic", 
       y = "95% CI width - logistic/linear",
       title = "a)")

cert_hf<-ggplot(boot_summary, 
              aes(x = CI_width_HF,
                  y = CI_width_HF_lin))+
  geom_point()+
  theme_classic()+
  xlim(0,1)+ylim(0,1)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "darkgray")+
  
  labs(x = "%95 CI width - logistic", 
       y = "",
       title = "b)")  


cert_plots<- cert_hd + cert_hf

ggsave("Figures/Supplemental/cert_lin_v_log.png",
       cert_plots, width = 8, height = 4)



