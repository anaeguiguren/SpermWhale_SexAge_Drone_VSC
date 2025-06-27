#peduncle dive summaries:
library(tidyr)
library(ggplot2)


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
  label = c("NB", "J", "SA", "AF", "AM/MF", "Fmax","MM"),
  x = 1
)


p<- ggplot(id.mean.p, aes(x = factor(pd_detected), y = mean_TL))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(fill = mean_fem_prob_hf),shape = 21, width = 0.25, alpha = 0.8, size =2)+
  scale_fill_wa_c("stuart", , limits = c(color_min, color_max)) +
  geom_hline(yintercept = whaling_lables$Length, colour = "gray", linetype = "dashed")+
  geom_text(data = whaling_lables, aes(y = Length+0.15, x = 0.5, label = label),
            hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "PD observed", y = "Mean Length (m)", fill = "P(fem)")

p
ggsave("Figures/boxplot_peduncle_dives.png",
       p, width = 3.5, height = 3.5)

#print summary statistics:

id.mean.p %>%
  group_by(pd_detected)%>%
  summarize(n = n(), 
            min_TL = min(mean_TL),
            max_TL = max(mean_TL))
