#Visualize -----
rm(list = ls())

load("bootstrapped_estimates.RData")
source("Scripts/functions.R")
library(wacolors)
library(patchwork)




#read in males:
morpho.males <- read.csv("Data/males_david.csv", header = T)

#save
#write.csv(boot_summary, "Data/Processed_Data/boot_summary_id.csv")

# add whaling references

whaling_lables_hd <- data.frame(
  Length = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), 
  Ratio = 0.74, 
  label = c("C", "J", "SA", "AF", "AM/MF", "Fmax","MM")
)

whaling_lables_hf <- data.frame(
  Length = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), 
  Ratio = 0.40, 
  label = c("C", "J", "SA", "AF", "AM/MF", "Fmax","MM")
)


# make individual lables dataframes:
#include suckling whale IDs and for sure male IDs (> 13.7 m)

boot_summary<-boot_summary %>% 
  mutate(short_ID = substr(ID, 10, 11), 
         label_show =
           ifelse(mean_length>13.7 | suckled_ever == TRUE,
                  short_ID, 
                  ""))


#make pd_seen column (giving/receiving)


boot_summary<- boot_summary %>%
  mutate(pd_detected = ifelse(suckling_ever == TRUE, "doing",
                              ifelse(suckled_ever == TRUE, "receiving", "no")))




# 1. Bootstrapped length and NR ratios -----

#set scale limits so that same legend can be applied to both plots:
# Color scales
color_min <- min(boot_summary$mean_fem_prob_hd, boot_summary$mean_fem_prob_hf, na.rm = TRUE)
color_max <- max(boot_summary$mean_fem_prob_hd, boot_summary$mean_fem_prob_hf, na.rm = TRUE)

# size scales
size_min <- min(boot_summary$CI_width_HD, boot_summary$CI_width_HF, na.rm = TRUE)
size_max <- max(boot_summary$CI_width_HD, boot_summary$CI_width_HF, na.rm = TRUE)

#~~~i. general summaries----

boot_summary$length_CI_width <- boot_summary$length_CI_hi- boot_summary$length_CI_low
hist(boot_summary$length_CI_width, breaks = 20)
mean(boot_summary$length_CI_width)
median(boot_summary$length_CI_width)
sd(boot_summary$length_CI_width)

plot(boot_summary$mean_length, boot_summary$length_CI_width)
plot(boot_summary$mean_length, boot_summary$length_CI_width/boot_summary$mean_length)
hist(boot_summary$length_CI_width/boot_summary$mean_length*100, breaks = 20)

median(boot_summary$length_CI_width/boot_summary$mean_length*100)
mean(boot_summary$length_CI_width/boot_summary$mean_length*100)
sd(boot_summary$length_CI_width/boot_summary$mean_length*100)


range(boot_summary$mean_length)
hist(boot_summary$mean_length, breaks = 20)
quantile(boot_summary$mean_length, probs = c(0.05, 0.95))

#widths:

mean(boot_summary$CI_width_R.HD)
sd(boot_summary$CI_width_R.HD)

mean(boot_summary$CI_width_R.HF)
sd(boot_summary$CI_width_R.HF)





#~~~ii. TL vs NR plots----

# make plots:


p0a<-ggplot(boot_summary, aes(x = mean_length, y = mean_R.HD))+
  geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+  # Vertical lines

  geom_pointrange(aes(ymin = R.HD_CI_low, ymax = R.HD_CI_hi), 
                  alpha = 0.7, size = 0.2)+
  geom_linerange(aes(xmin = length_CI_low, xmax = length_CI_hi), alpha = 0.7)+
  theme_classic()+
  geom_text(data = whaling_lables_hf, aes(x = Length+0.1, label = label),
            y = 0.728,
            hjust = 0, size = 2.5, inherit.aes = F)+
  labs(title = "a)",
       x = "Length (m)",
       y = expression(NR[dorsal]),      
       fill = "P(f)",
       size = "95%CI width",
       shape = "Suckled")+
  theme(legend.position = "null")



p0b<-ggplot(boot_summary, aes(x = mean_length, y = mean_R.HF))+
  geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+  # Vertical lines
  
  geom_pointrange(aes(ymin = R.HF_CI_low, ymax = R.HF_CI_hi), 
                  alpha = 0.7, size =0.2)+
  geom_linerange(aes(xmin = length_CI_low, xmax = length_CI_hi), alpha = 0.7)+
  theme_classic()+
  geom_text(data = whaling_lables_hf, aes(x = Length+0.1, label = label),
            y = 0.425,
            hjust = 0, size = 2.5, inherit.aes = F)+
  labs(title = "b)",
       x = "Length (m)",
       y = expression(NR[flipper]),      
       fill = "P(f)",
       size = "95%CI width",
       shape = "Suckled")+
  theme(legend.position = "null")


comb0 <- p0a + p0b
comb0

ggsave("Figures/bootstrap_length_NRS.png",
       comb0, width = 9, height = 4)

ggsave("Figures/bootstrap_length_NR_flipper.png",
       p0b+labs(title = ""), width =7 , height = 4)


#~~~iii. NR variability plots----


boot_summary$CI_width_R.HD
boot_summary$CI_width_R.HF

#make centered values:
centered_df<-boot_summary%>%
  arrange(mean_R.HD)%>%
  mutate(ind = seq_along(ID),
         mean_R.HD.center = mean_R.HD - mean(mean_R.HD), #center NR values around their mean
         R.HD_CI_low.center = R.HD_CI_low - mean_R.HD,
         R.HD_CI_hi.center = R.HD_CI_hi- mean_R.HD,
         R.HD.CI_width = unname(R.HD_CI_hi-R.HD_CI_low),
         
         mean_R.HF.center = mean_R.HF - mean(mean_R.HF),
         R.HF_CI_low.center = R.HF_CI_low- (mean_R.HF),
         R.HF_CI_hi.center = R.HF_CI_hi - (mean_R.HF),
         R.HF.CI_width = unname(R.HF_CI_hi - R.HF_CI_low)
         
         
         )


#how many are wider: 
centered_df$R.HD.CI_width


centered_df %>%
  mutate(R.HD_wider = ifelse(
    R.HD.CI_width > R.HF.CI_width, "yes", "no"
  ))%>%
  group_by(R.HD_wider)%>%
  summarize(n = n())



#make convert to long form:
hd_long<-centered_df %>%
  select(ind, mean_length, mean = mean_R.HD.center, low = R.HD_CI_low.center, hi = R.HD_CI_hi.center)%>%
  mutate(Measure = "NRdorsal")


hf_long<-centered_df %>%
  select(ind, mean_length, mean = mean_R.HF.center, low = R.HF_CI_low.center, hi = R.HF_CI_hi.center)%>%
  mutate(Measure = "NRflipper")


#combine:
long_centered <- bind_rows(hd_long, hf_long)


ci.w<-long_centered %>%
  mutate(width = hi - low) %>%
  ggplot(aes(x = Measure, y = width)) +
  geom_boxplot(alpha = 0.6) +
  scale_x_discrete(
    labels = c(
      "NRflipper" = expression(NR[flipper]), 
      "NRdorsal" = expression(NR[dorsal])
    )
  )+
  geom_jitter(width = 0.1, alpha = 0.4) +
  scale_fill_grey(start = 0.3, end = 0.7) +
  labs(y = "95% Confidence interval width", x = "") +
  theme_bw()

ggsave("Figures/boxplot_NR_CI_Widths_supplementary.png",
       ci.w, width = 5, height = 5)

ci.w<-long_centered %>%
  mutate(width = hi - low)

# 2. Bootstrapped p_f for models based on HD and HF----
#~~~~i. plot ----
library(ggrepel)
  

#highlight very certain points:
boot_summary$high_cert_HD <- ifelse(boot_summary$CI_width_HD<= 0.05, "cert", "uncert")

boot_summary$high_cert_HF <- ifelse(boot_summary$CI_width_HF<= 0.05, "cert", "uncert")
  
p1 <- ggplot(boot_summary, aes(x = mean_length, y = mean_R.HD)) +
  geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed") + 
  geom_point(aes(colour = mean_fem_prob_hd, fill = mean_fem_prob_hd,
                 shape = factor(pd_detected)), size = 2, alpha = 0.9) +
  # Black outline only for "cert" points
  geom_point(data = subset(boot_summary, high_cert_HD == "cert"),
             aes(x = mean_length, y = mean_R.HD, 
                 shape = factor(pd_detected)),
             color = "black", stroke = 1, size = 2, fill = NA, inherit.aes = FALSE) +
  geom_text_repel(aes(label = label_show), 
                  box.padding = 1, alpha = .8, max.overlaps = Inf, size = 3) +
  scale_fill_wa_c("puget", limits = c(color_min, color_max), reverse = T) +
  scale_color_wa_c("puget", limits = c(color_min, color_max), reverse = T) +
  scale_size(limits = c(size_min, size_max)) +
  scale_shape_manual(values = c("no" = 21, "receiving" = 24, "doing" = 22)) +
  theme_classic() +
  geom_text(data = whaling_lables_hf, aes(x = Length + 0.1, label = label),
            hjust = 0, y = 0.71, size = 2.5, inherit.aes = FALSE) +
  labs(title = "b)",
       x = "Length (m)",
       y = expression(NR[flipper]),      
       fill = "P(f)",
       size = "95% CI width",
       shape = "PD observed")+theme(legend.position = "none")


p2 <- ggplot(boot_summary, aes(x = mean_length, y = mean_R.HF)) +
  geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed") + 
  geom_point(aes(colour = mean_fem_prob_hf, fill = mean_fem_prob_hf,
                 shape = factor(pd_detected)), size = 2, alpha = 0.9) +
  # Black outline only for "cert" points
  geom_point(data = subset(boot_summary, high_cert_HF == "cert"),
             aes(x = mean_length, y = mean_R.HF, 
                 shape = factor(pd_detected)),
             color = "black", stroke = 1, size = 2, fill = NA, inherit.aes = FALSE) +
  geom_text_repel(aes(label = label_show), 
                  box.padding = 1, alpha = .8, max.overlaps = Inf, size = 3) +
  scale_fill_wa_c("puget", limits = c(color_min, color_max), reverse = T) +
  scale_color_wa_c("puget", limits = c(color_min, color_max), reverse = T) +
  scale_size(limits = c(size_min, size_max)) +
  scale_shape_manual(values = c("no" = 21, "receiving" = 24, "doing" = 22)) +
  theme_classic() +
  geom_text(data = whaling_lables_hf, aes(x = Length + 0.1, label = label),
            hjust = 0, y = 0.412, size = 2.5, inherit.aes = FALSE) +
  labs(title = "b)",
       x = "Length (m)",
       y = expression(NR[flipper]),      
       fill = "P(f)",
       size = "95% CI width",
       shape = "PD observed")+guides(colour = "none")


comb <- p1 + p2
comb 

ggsave("Figures/bootstrap_post_prob_models.png",
       comb, width = 9, height = 4)


ggsave("Figures/bootstrap_post_prob_models_HF.png",
       p2 + labs(title = ""), width = 8, height = 4)


#~~~~ii. summarize -----

# 2. Parameter estimates ----
#~~~~a. plot----
library(ggdist)
library(distributional)

#reframe data for plotting
#hd metrics

hd_params_df$boot <-1:nrow(hd_params_df)

hd_params<-pivot_longer(data = hd_params_df, 
             cols = -boot, 
             names_to = "parameters", 
             values_to = "estimate")

hd_params$method <- "R - Dorsal"
#hf metrics
hf_params_df$boot <-1:nrow(hf_params_df)

hf_params<-pivot_longer(data = hf_params_df, 
                        cols = -boot, 
                        names_to = "parameters", 
                        values_to = "estimate")

hf_params$method <- "R - Flipper"

#combine:
all_params <- bind_rows(hd_params, hf_params)

param_lables <- c("A. maxf", "B. fr", "C. maxm", "D. mr")

param_plot<- all_params %>%
  ggplot(aes(x = method, y = estimate))+
  geom_boxplot(alpha = 0.8)+
  scale_x_discrete(
    labels = c(
      "R - Flipper" = expression(NR[flipper]), 
      "R - Dorsal" = expression(NR[dorsal])
    )
  )+
  coord_flip()+
  facet_wrap(~parameters, scales = "free_x", ncol =2)+
  labs(y = "Estimate", x = "NR Metric")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "null")
  

param_plot

ggsave("Figures/bootstrap_params_models.png",
       param_plot, width = 7, height = 3.5)

#nrflipper only 


param_lables <- c("A. maxf", "B. fr", "C. maxm", "D. mr")

hf_param_plot <- hf_params %>%
  ggplot(aes(x = parameters, y = estimate)) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~parameters, scales = "free", ncol = 4) +
  labs(y = "Estimate", x = "Parameter") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())  



ggsave("Figures/bootstrap_params_models_HF.png",
       hf_param_plot, width = 7, height = 3.5)


#~~~~b. table----

hd_params_df$method <- "HD"
hf_params_df$method <- "HF"

all_params_wide <- rbind(hd_params_df, hf_params_df)

# summarize: 
#detach(package:ggpubr, unload = TRUE)

all_params_wide %>%
  dplyr::group_by(method) %>%
  dplyr::summarize(
    fr = mean(fr),
    fr_low = unname(quantile(fr, 0.025)))



estimate_summary <- function(df) {
  result <- by(df, df$method, function(subset) {
    data.frame(
      method = unique(subset$method),
      
      #fr
      fr_mean = mean(subset$fr),
      fr_low = unname(quantile(subset$fr, 0.025)),
      fr_high = unname(quantile(subset$fr, 0.975)),
      
      #fmax
      fmax_mean = mean(subset$fmax),
      fmax_low = unname(quantile(subset$fmax, 0.025)),
      fmax_high = unname(quantile(subset$fmax, 0.975)),
      
      #mr
      mr_mean = mean(subset$mr),
      mr_low = unname(quantile(subset$mr, 0.025)),
      mr_high = unname(quantile(subset$mr, 0.975)),
      
      #mmax
      mmax_mean = mean(subset$mmax),
      mmax_low = unname(quantile(subset$mmax, 0.025)),
      mmax_high = unname(quantile(subset$mmax, 0.975))
    )
  })
  
  do.call(rbind, result)
}

result_df <- estimate_summary(all_params_wide)


# make pretty:

# Assuming you've already created result_df with estimate_summary()

polished_df <- data.frame(
  ratio_measurement = result_df$method,
  
  fr = paste0(
    round(result_df$fr_mean, 2), " (",
    round(result_df$fr_low, 2), " - ",
    round(result_df$fr_high, 2), ")"
  ),
  
  fmax = paste0(
    round(result_df$fmax_mean, 2), " (",
    round(result_df$fmax_low, 2), " - ",
    round(result_df$fmax_high, 2), ")"
  ),
  
  mr = paste0(
    round(result_df$mr_mean, 2), " (",
    round(result_df$mr_low, 2), " - ",
    round(result_df$mr_high, 2), ")"
  ),
  
  mmax = paste0(
    round(result_df$mmax_mean, 2), " (",
    round(result_df$mmax_low, 2), " - ",
    round(result_df$mmax_high, 2), ")"
  )
)



#save
library(flextable)
library(officer)

# Export to Word document

ft <- flextable(polished_df)
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "Figures/bootstra_parameter_table.docx")

#~~~~c. curves -----



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


# squish lists into data frames:

f_lines_hd <- do.call(rbind, all_f_lines_hd)
m_lines_hd <- do.call(rbind, all_m_lines_hd)

# make single data frame
all_lines_hd <- rbind(f_lines_hd, m_lines_hd)

summary(all_lines_hd)
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


# store for plot


mean_f_line_hd <- data.frame(Length = f_mean_line_hd$Length,
                          Ratio = f_mean_line_hd$Ratio, Sex = "Fem", Bootstrap = 1)

mean_m_line_hd <- data.frame(Length = m_mean_line_hd$Length,
                          Ratio = m_mean_line_hd$Ratio, Sex = "Mal", Bootstrap = 1)

mean_lines_hd <- rbind(mean_f_line_hd, mean_m_line_hd)


#~~~~~~~ii.HF-----
#loop through:
all_f_lines_hf <- list()
all_m_lines_hf <- list()

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
  
  f_line_hf <- data.frame(Length = x_F, Bootstrap = i, Sex = "F", Ratio = y_F)
  m_line_hf <- data.frame(Length = x_M, Bootstrap = i, Sex = "M",Ratio = y_M)
  
  #store in big list
  all_f_lines_hf[[i]] <- f_line_hf
  all_m_lines_hf[[i]] <- m_line_hf
  
}


# squish lists into data frames:

f_lines_hf <- do.call(rbind, all_f_lines_hf)
m_lines_hf <- do.call(rbind, all_m_lines_hf)

# make single data frame
all_lines_hf <- rbind(f_lines_hf, m_lines_hf)

summary(all_lines_hf)
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


# store for plot


mean_f_line_hf <- data.frame(Length = f_mean_line_hf$Length,
                             Ratio = f_mean_line_hf$Ratio, Sex = "Fem", Bootstrap = 1)

mean_m_line_hf <- data.frame(Length = m_mean_line_hf$Length,
                             Ratio = m_mean_line_hf$Ratio, Sex = "Mal", Bootstrap = 1)

mean_lines_hf <- rbind(mean_f_line_hf, mean_m_line_hf)


#~~~~~~~iii.plot----

#HD

p3<-ggplot(all_lines_hd, aes(x = Length, y = Ratio, 
                         group = interaction(Bootstrap, Sex),
                         colour = Sex))+
  #geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+  # Vertical lines
  geom_vline(xintercept = 6, alpha = 0.3)+  geom_line(alpha = 0.05, linewidth = 0.5)+
  geom_line(data = mean_f_line_hd, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth = 1, alpha = 1) +  # mean lines
  geom_line(data = mean_m_line_hd, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth =1, alpha = 1, linetype = "dashed") +  # mean lines
  scale_color_manual(values = c("F" = "#123c2e", "M" = "#eba8ad", 
                                "Fem" = "#b8c5c0", "Mal" = "#f9e5e6"))+
  scale_y_continuous(limits = c(0.56, 0.74))+
  #geom_text(data = whaling_lables_hd, aes(x = Length+0.1,label = label),
  #          y =0.745,
  #          hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "Length (m)", y = expression(NR[dorsal]), title = "a)")+
  theme(legend.position = "null")
p3
#HF

p4<-ggplot(all_lines_hf, aes(x = Length, y = Ratio, 
                             group = interaction(Bootstrap, Sex),
                             colour = Sex))+
  #geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+  # Vertical lines
  geom_vline(xintercept = 6, alpha = 0.3)+
  geom_line(alpha = 0.05, linewidth = 0.5)+
  geom_line(data = mean_f_line_hf, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth = 1, alpha = 1) +  # mean lines
  geom_line(data = mean_m_line_hf, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth =1, alpha = 1 ,linetype = "dashed") +  # mean lines

  scale_color_manual(values = c("F" = "#123c2e", "M" = "#eba8ad", 
                                "Fem" = "#b8c5c0", "Mal" = "#f9e5e6"))+
  scale_y_continuous(limits = c(0.24, 0.42))+
  #geom_text(data = whaling_lables_hf, aes(x = Length+0.1, label = label),
  #          y = 0.425,
  #          hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "Length (m)", y = expression(NR[flipper]), title = "b)")+
  guides(colour = "none")
 

curves <- p3 + p4

curves
ggsave("Figures/bootstrap_params_curves.png",
       curves, width = 8, height = 4)

ggsave("Figures/bootstrap_params_curves_HF.png",
       p4+labs(title = ""), width = 7, height = 4)
ggsave("Figures/bootstrap_params_curves_HD.png",
       p3+labs(title = ""), width = 7, height = 4)

# 7. Bootstrapped p(f) + mean curves----


p5  <-ggplot(boot_summary, aes(x = mean_length, y = mean_R.HD))+
  geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+  # Vertical lines
  geom_line(data = mean_f_line_hd %>% filter(Length < 12), aes(x = Length, y = Ratio), 
            linewidth = 1, alpha = 1, colour = "#123c2e") +  # mean lines
  geom_line(data = mean_m_line_hd, aes(x = Length, y = Ratio), 
            linewidth =1, alpha = 1, linetype = "dashed", colour = "#eba8ad") +  # mean lines
  geom_point(aes(colour = mean_fem_prob_hd, fill = mean_fem_prob_hd,
                 shape = factor(pd_detected)), size = 2, alpha = 0.8) +
  #geom_text_repel(aes(label = label_show), 
   #               box.padding = 1, alpha = .8, max.overlaps = Inf, size = 3) +
  # Black outline only for "cert" points
  geom_point(data = subset(boot_summary, high_cert_HD == "cert"),
             aes(x = mean_length, y = mean_R.HD, 
                 shape = factor(pd_detected), fill = NULL),
             color = "black", stroke = 1, size = 2,  inherit.aes = FALSE) +
  
  scale_fill_wa_c("puget", limits = c(color_min, color_max), reverse = T) +
  scale_color_wa_c("puget", limits = c(color_min, color_max), reverse = T) +
  scale_size(limits = c(size_min, size_max))+
  scale_shape_manual(values = c("no" = 21, "receiving" = 24, "doing" = 22))+
  theme_classic()+
  geom_text(data = whaling_lables_hd, aes(x = Length+0.1, label = label),
            y = 0.71,
            hjust = 0, size = 2.5, inherit.aes = F)+
  
  labs(title = "a)",
       x = "Length (m)",
       y = expression(NR[dorsal]),      
       fill = "P(f)",
       size = "95%CI width",
       shape = "Suckled")+
  theme(legend.position = "null")



p6<-ggplot(boot_summary, aes(x = mean_length, y = mean_R.HF))+
  geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+ 
  geom_line(data = mean_f_line_hf %>% filter(Length < 12), aes(x = Length, y = Ratio), 
            linewidth = 1, alpha = 1, colour = "#123c2e") +  # mean lines
  geom_line(data = mean_m_line_hf, aes(x = Length, y = Ratio), 
            linewidth =1, alpha = 1, linetype = "dashed", colour = "#eba8ad") +  # mean lines
  
  geom_point(aes(colour = mean_fem_prob_hf, fill = mean_fem_prob_hf,
                 shape = factor(pd_detected)), size = 2, alpha = 0.8) +
 # geom_text_repel(aes(label = label_show), 
  #                box.padding = 1, alpha = .8, max.overlaps = Inf, size = 3) +
  # Black outline only for "cert" points
  geom_point(data = subset(boot_summary, high_cert_HF == "cert"),
             aes(x = mean_length, y = mean_R.HF, 
                 shape = factor(pd_detected), fill = NULL),
             color = "black", stroke = 1, size = 2,  inherit.aes = FALSE) +
  
  scale_fill_wa_c("puget", limits = c(color_min, color_max), reverse = T) +
  scale_color_wa_c("puget", limits = c(color_min, color_max), reverse = T) +
  scale_shape_manual(values = c("no" = 21, "receiving" = 24, "doing" = 22))+
  #add males from Gully & arctic
  geom_point(data = morpho.males, 
             aes(x = TL.m, y = R.HF.m), size = 2, shape = 8, inherit.aes = F)+
  
  theme_classic()+
  geom_text(data = whaling_lables_hf, aes(x = Length+0.1, label = label),
            y = 0.413,
            hjust = 0, size =2.5, inherit.aes = F)+
  
  labs(title = "b)",
       x = "Length (m)",
       y = expression(NR[flipper]),      
       fill = "P(f)",
       size = "95% CI width",
       shape = "PD observed")+ guides(colour = "none")


comb2 <- p5 + p6
comb2 

ggsave("Figures/bootstrap_post_prob_models_mean_curves.png",
       comb2, width = 9, height = 4)


ggsave("Figures/bootstrap_post_prob_models_mean_curves_HD.png",
       p5+labs(title = "")+guides(colour = "none"), width = 7, height = 4)


ggsave("Figures/bootstrap_post_prob_models_mean_curves_HF_notxt.png",
       p6+labs(title = "")+guides(colour = "none"), width = 7, height = 4)



# HF. Only plots-----
comb3 <- (p4+labs(title = "a)")) / p6

ggsave("Figures/bootstrap_post_prob_models_mean_curves_HF_2panel.png", 
       comb3, width = 8, height = 8)




# 6. visualize individual - level variations----
boot_summary%>%
  arrange(mean_fem_prob_hd)%>%
  mutate(ind = seq_along(ID))%>%
  ggplot(aes(x = ind, y = mean_fem_prob_hd, colour = mean_fem_prob_hd))+
  scale_color_wa_c("puget", , limits = c(color_min, color_max)) +
  geom_errorbar(aes(ymin = prob_hd_CI_low, ymax = prob_hd_CI_hi ))+
  geom_point()+
  theme_classic()


boot_summary%>%
  arrange(mean_fem_prob_hf)%>%
  mutate(ind = seq_along(ID))%>%
  ggplot(aes(x = ind, y = mean_fem_prob_hf, colour = mean_fem_prob_hf))+
  scale_color_wa_c("puget", , limits = c(color_min, color_max)) +
  geom_errorbar(aes(ymin = prob_hf_CI_low, ymax = prob_hf_CI_hi ))+
  geom_point()+
  theme_classic()

