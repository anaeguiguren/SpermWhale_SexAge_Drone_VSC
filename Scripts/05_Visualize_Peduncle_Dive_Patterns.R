#peduncle dive summaries:
library(tidyr)
library(ggplot2)
library(dplyr)


#load data
id.mean<-read.csv("Data/Processed_Data/id_morpho_output_clean_processed.csv")

load("bootstrapped_estimates.RData")


#set scale limits so that same legend can be applied to both plots:
# Color scales
color_min <- min(boot_summary$mean_fem_prob_hd, boot_summary$mean_fem_prob_hf, na.rm = TRUE)
color_max <- max(boot_summary$mean_fem_prob_hd, boot_summary$mean_fem_prob_hf, na.rm = TRUE)






id.mean.p<-left_join(id.mean, boot_summary, by = "ID")
#make pd_seen column (giving/receiving)


id.mean.p<- id.mean.p %>%
  mutate(pd_detected = ifelse(suckling_ever.x == TRUE, "doing",
                              ifelse(suckled_ever.x == TRUE, "receiving", "no")))

id.mean.p$pd_detected <- factor(id.mean.p$pd_detected, levels = c("doing", "receiving", "no"))



# age lables
whaling_lables <- data.frame(
  Length = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), 
  label = c("C", "J", "SA", "AF", "AM/MF", "Fmax","MM"),
  x = 1
)




#PD observed vs. length -----

#make a shape column:

id.mean.p$HF_prob_CI_width<- id.mean.p$prob_hf_CI_hi - id.mean.p$prob_hf_CI_low

id.mean.p$cert_HF <- ifelse(id.mean.p$HF_prob_CI_width<=0.05, "cert", "uncert")

id.mean.p<- id.mean.p %>%
  mutate(point_shape = ifelse(is.na(mean_fem_prob_hf), "known","unknown"))

p<- ggplot(id.mean.p, aes(x = factor(pd_detected), y = mean_TL))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(fill = mean_fem_prob_hf, shape = point_shape, colour=mean_fem_prob_hf), 
              width = 0.25, alpha = 0.8, size =2)+
 # geom_jitter(data = subset(id.mean.p, cert_HF == "cert"),
  #            aes(y = mean_TL, fill = mean_fem_prob_hf, shape = point_shape), 
   #           colour = "gray30",
    #          width = 0.25, alpha = 0.8, size =3)+
  scale_shape_manual(values = c("unknown" = 21, "known" = 8))+
  scale_fill_wa_c("diablo", reverse = T, limits = c(color_min, color_max)) +
  scale_color_wa_c("diablo", reverse = T, limits = c(color_min, color_max)) +
  geom_hline(yintercept = whaling_lables$Length, colour = "gray", linetype = "dashed")+
  geom_text(data = whaling_lables, aes(y = Length+0.15, x = 0.5, label = label),
            hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "PD observed", y = "Length (m)", fill = "P(f)")+guides(colour = "none")

p


ggsave("Figures/boxplot_peduncle_dives.png",
       p, width =7, height = 4)



#p_fem by age class:

#age classes: 
id.mean.p <- id.mean.p %>%
  mutate(lit.sex.age=
           ifelse(mean_TL<5.5, "calf", 
                  ifelse(mean_TL < 7.5, "juvenile", 
                         ifelse(mean_TL < 12.5, "adfem_juv", 
                                ifelse(mean_TL < 13.7 , "adult_male", 
                                       "mature_male")))))%>%
  mutate(lit.sex.age = factor(lit.sex.age, levels = c("calf", "juvenile", "adfem_juv", "adult_male", "mature_male")))



ggplot(id.mean.p, aes(x = factor(lit.sex.age), y = mean_fem_prob_hf, colour = pd_detected))+
  geom_jitter()
  #geom_jitter()


#print summary statistics:

id.mean.p %>%
  group_by(pd_detected)%>%
  summarize(n = n(), 
            min_TL = min(mean_TL),
            max_TL = max(mean_TL))
