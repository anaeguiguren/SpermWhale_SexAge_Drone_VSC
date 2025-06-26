# 4. Visualize -----
rm(list = ls())

load("bootstrapped_estimates.RData")
source("Scripts/functions.R")
library(wacolors)
library(patchwork)

# 1. Bootstrapped p_f for models based on HD and HF----
#compute confidence interval width for (p(f)) and p(m)
#female
boot_summary$CI_width_HD <- boot_summary$prob_hd_CI_hi - boot_summary$prob_hd_CI_low
boot_summary$CI_width_HF <- boot_summary$prob_hf_CI_hi - boot_summary$prob_hf_CI_low

#male
boot_summary$CI_width_HD_male <- boot_summary$m_prob_hd_CI_hi - boot_summary$m_prob_hd_CI_low
boot_summary$CI_width_HF_male <- boot_summary$m_prob_hf_CI_hi - boot_summary$m_prob_hf_CI_low
#they are equivalent -leave them be!

#set scale limits so that same legend can be applied to both plots:
# Color scales
color_min <- min(boot_summary$mean_fem_prob_hd, boot_summary$mean_fem_prob_hf, na.rm = TRUE)
color_max <- max(boot_summary$mean_fem_prob_hd, boot_summary$mean_fem_prob_hf, na.rm = TRUE)

# size scales
size_min <- min(boot_summary$CI_width_HD, boot_summary$CI_width_HF, na.rm = TRUE)
size_max <- max(boot_summary$CI_width_HD, boot_summary$CI_width_HF, na.rm = TRUE)

# make plots:


# add whaling references

whaling_lables_hd <- data.frame(
  Length = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), 
  Ratio = 0.56, 
  label = c("NB", "J", "SA", "AF", "AM/MF", "Fmax","MM")
)

whaling_lables_hf <- data.frame(
  Length = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), 
  Ratio = 0.24, 
  label = c("NB", "J", "SA", "AF", "AM/MF", "Fmax","MM")
)


# make individual lables dataframes:
#include suckling whale IDs and for sure male IDs (> 13.7 m)

boot_summary<-boot_summary %>% 
  mutate(short_ID = substr(ID, 10, 11), 
         label_show =
           ifelse(mean_length>13.7 | suckled_ever == TRUE,
                  short_ID, 
                  ""))

#~~~~i. plot ----
library(ggrepel)
  
  
p1 <- 
  ggplot(boot_summary, aes(x = mean_length, y = mean_R.HD))+
  geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+  # Vertical lines
  geom_point(aes(fill = mean_fem_prob_hd, size = CI_width_HD, 
                 shape = factor(suckled_ever), alpha = 1.5), alpha = 0.8)+
  scale_fill_wa_c("stuart", , limits = c(color_min, color_max)) +
  scale_size(limits = c(size_min, size_max))+
  scale_shape_manual(values = c("FALSE" = 21, "TRUE" = 24))+
  geom_label_repel(aes(label = label_show, fill =mean_fem_prob_hd ), 
                   box.padding = 1, alpha = .8, max.overlaps = Inf, label.padding = 0.15, size = 2.5)+
  theme_classic()+
  geom_text(data = whaling_lables_hd, aes(x = Length+0.1, y = Ratio, label = label),
            hjust = 0, size = 2.5, inherit.aes = F)+

  labs(title = "A",
       x = "Length (m)",
       y = "R - Dorsal",
       fill = "P(fem)",
       size = "95-CI width",
       shape = "Suckled")+
  theme(legend.position = "null")


 p2<-ggplot(boot_summary, aes(x = mean_length, y = mean_R.HF))+
  geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+ 
  geom_point(aes(fill = mean_fem_prob_hf, size = CI_width_HF, 
                 shape = factor(suckled_ever)), alpha = 0.9)+
  geom_label_repel(aes(label = label_show, fill =mean_fem_prob_hf), 
                    box.padding = 1, alpha = .8, max.overlaps = Inf, size =2.5, label.padding =  0.15)+
  scale_fill_wa_c("stuart", , limits = c(color_min, color_max)) +
  scale_size(limits = c(size_min, size_max))+
  scale_shape_manual(values = c("FALSE" = 21, "TRUE" = 24))+
  theme_classic()+
  geom_text(data = whaling_lables_hf, aes(x = Length+0.1, y = Ratio, label = label),
            hjust = 0, size =2.5, inherit.aes = F)+
 
  labs(title = "B",
       x = "Length (m)",
       y = "R - Flipper",
       fill = "P(fem)",
       size = "95-CI width",
       shape = "Suckled")


comb <- p1 + p2
comb 

ggsave("Figures/bootstrap_post_prob_models.png",
       comb, width = 10, height = 4)

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
hd_params_hf$boot <-1:nrow(hd_params_hf)

hf_params<-pivot_longer(data = hd_params_hf, 
                        cols = -boot, 
                        names_to = "parameters", 
                        values_to = "estimate")

hf_params$method <- "R - Flipper"

#combine:
all_params <- bind_rows(hd_params, hf_params)

param_lables <- c("A. maxf", "B. fr", "C. maxm", "D. mr")

param_plot<- all_params %>%
  ggplot(aes(x = method, y = estimate, colour = method))+
  geom_boxplot(alpha = 0.8)+
  coord_flip()+
  facet_wrap(~parameters, scales = "free_x", ncol =2)+
  scale_color_wa_d("lopez") +
  labs(y = "Estimate", x = "R Metric")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "null")
  

param_plot

ggsave("Figures/bootstrap_params_models.png",
       param_plot, width = 7, height = 3.5)




#~~~~b. table----

hd_params_df$method <- "HD"
hd_params_hf$method <- "HF"

all_params_wide <- rbind(hd_params_df, hd_params_hf)

# summarize: 
detach(package:ggpubr, unload = TRUE)

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
x_M <- seq(min.L, max.L.M, by = 0.2)

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

for(i in 1:nrow(hd_params_hf)){
  
  y_F <- fem_curve(length = x_F,
                   fr = hd_params_hf$fr[i],
                   fmax = hd_params_hf$fmax[i])
  
  
  y_M <- mal_curve(length = x_M,
                   fr = hd_params_hf$fr[i],
                   fmax = hd_params_hf$fmax[i], 
                   mr = hd_params_hf$mr[i], 
                   mmax = hd_params_hf$mmax[i],
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
  geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+  # Vertical lines
  geom_line(alpha = 0.3, linewidth = 0.5)+
  geom_line(data = mean_lines_hd, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth = 1, alpha = 0.7) +  # mean lines
  scale_color_manual(values = c("F" = "#344e37", "M" = "#603b79", 
                                "Fem" = "#74a278", "Mal" = "#aa7dc7"))+
  scale_y_continuous(limits = c(0.56, 0.74))+
  geom_text(data = whaling_lables_hd, aes(x = Length+0.1, y = Ratio, label = label),
            hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "Length (m)", y = "R - Dorsal", title = "A")+
  theme(legend.position = "null")

#HF

p4<-ggplot(all_lines_hf, aes(x = Length, y = Ratio, 
                             group = interaction(Bootstrap, Sex),
                             colour = Sex))+
  geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+  # Vertical lines
  geom_line(alpha = 0.3, linewidth = 0.5)+
  geom_line(data = mean_lines_hf, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth = 1, alpha = 0.7) +  # mean lines
  scale_color_manual(values = c("F" = "#344e37", "M" = "#603b79", 
                                "Fem" = "#74a278", "Mal" = "#aa7dc7"))+
  scale_y_continuous(limits = c(0.24, 0.42))+
  geom_text(data = whaling_lables_hf, aes(x = Length+0.1, y = Ratio, label = label),
            hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "Length (m)", y = "R - Flipper", title = "B", 
       colour = "Sex")+
  theme(legend.position = "null")
  



curves <- p3 + p4

#curves
ggsave("Figures/bootstrap_params_curves.png",
       curves, width = 8, height = 4)


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
