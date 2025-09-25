#peduncle dive summaries:
rm(list = ls())

source("Scripts/functions.R")
library(wacolors)
library(patchwork)
library(ggrepel)

#1. Load and prep data -----

#measures from all whales (also those without enough R.HF measures)
id.mean<-read.csv("Data/Processed_Data/id_morpho_output_clean_processed.csv")

# only whales with known hf - and p_hf estimates
load("bootstrapped_estimates_plus_mean.RData")
head(dat)

# bootstrapped CI widths per individual 
head(boot_summary)

# get only relevant columns
boot_summary<- boot_summary %>%
  select(ID, CI_width_HD, CI_width_HF)



# join data frames  

# add CI width data to id.mean
id.mean.p<-left_join(id.mean, boot_summary, by = "ID")

# add pf data to id.mean.p

id.mean.p <- left_join(id.mean.p, dat, by = "ID")


#make pd_seen column (giving/receiving)


id.mean.p<- id.mean.p %>%
  mutate(pd_detected = ifelse(suckling_ever.x == TRUE, "doing",
                              ifelse(suckled_ever.x == TRUE, "receiving", "no")))

id.mean.p$pd_detected <- factor(id.mean.p$pd_detected, levels = c("doing", "receiving", "no"))


# 2. Create visual elements -----
# age labels
whaling_lables <- data.frame(
  Length = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), 
  label = c("C", "J", "SA", "AF", "AM/MF", "Fmax","MM"),
  x = 1
)

# create certainty columns

id.mean.p$cert_HF <- ifelse(id.mean.p$CI_width_HF<=0.05, "cert", "uncert")

id.mean.p<- id.mean.p %>%
  mutate(point_shape = ifelse(!is.na(P_fem_HF), "known","unknown"))

# Color scales for p_fem
color_min <- min(id.mean.p$P_fem_HF, na.rm = TRUE)
color_max <- max(id.mean.p$P_fem_HF, na.rm = TRUE)


p<- ggplot(id.mean.p, aes(x = factor(pd_detected), y = mean_TL))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(fill = P_fem_HF, shape = point_shape, colour=P_fem_HF), 
              width = 0.25, alpha = 0.8, size =2)+
 #geom_jitter(data = subset(id.mean.p, cert_HF == "cert"),
  #            aes(y = mean_TL, fill = P_fem_HF, shape = point_shape), 
   #           colour = "black",
    #          width = 0.25, alpha = 0.8, size =2, stroke = 1.5)+
  scale_shape_manual(values = c("known" = 21, "unknown" = 8))+
  scale_fill_wa_c("puget", reverse = T, limits = c(color_min, color_max)) +
  scale_color_wa_c("puget", reverse = T, limits = c(color_min, color_max)) +
  geom_hline(yintercept = whaling_lables$Length, colour = "gray", linetype = "dashed")+
  geom_text(data = whaling_lables, aes(y = Length+0.15, x = 0.5, label = label),
            hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "PD observed", y = "Length (m)", fill = "P(f)")+guides(colour = "none")

p


ggsave("Figures/Final_Figures/Fig7_boxplot_peduncle_dives.png",
       p, width =7, height = 4)

id.mean.p %>%
  filter(pd_detected == "receiving", 
         point_shape == "known", 
         cert_HF == "uncert")

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




#print summary statistics:

id.mean.p %>%
  group_by(pd_detected)%>%
  summarize(n = n(), 
            min_TL = min(mean_TL),
            max_TL = max(mean_TL))
