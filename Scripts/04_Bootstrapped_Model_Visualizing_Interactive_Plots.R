library(ggplot2)
library(plotly)



p2.y <- ggplot(boot_summary, aes(x = mean_length, y = mean_R.HF, fill = mean_fem_prob_hf)) +
  
  geom_point(aes(
    size = CI_width_HF,
    shape = factor(suckled_ever),
    text = paste0(
      "Length: ", round(mean_length, 2), " m",
      "<br>Ratio: ", round(mean_R.HF, 3),
      "<br>P(fem): ", round(mean_fem_prob_hf, 2),
      "<br>CI width: ", round(CI_width_HF, 3),
      "<br>Suckled: ", suckled_ever
    )),
    alpha = 0.9
  ) +
  
  labs(title = "B",
       x = "Length (m)",
       y = "Ratio (rostrum - flipper base)",
       fill = "P(fem)",
       size = "95-CI width",
       shape = "Suckled")

# Now convert to interactive with tooltip
p2.py <- ggplotly(p2.y, tooltip = "text")
p2.py


#females:

summary(boot_summary$mean_length[which(boot_summary$mean_fem_prob_hf > 0.95 & boot_summary$CI_width_HF < 0.05)])
summary(boot_summary$mean_R.HF[which(boot_summary$mean_fem_prob_hf > 0.95 & boot_summary$CI_width_HF < 0.05)])

#mature males:
summary(boot_summary$mean_length[which(boot_summary$mean_fem_prob_hf < 0.05 & boot_summary$CI_width_HF < 0.05)])
summary(boot_summary$mean_R.HF[which(boot_summary$mean_fem_prob_hf < 0.05 & boot_summary$CI_width_HF < 0.05)])
length(boot_summary$mean_R.HF[which(boot_summary$mean_fem_prob_hf < 0.05 & boot_summary$CI_width_HF < 0.05)])




#females:

summary(boot_summary$mean_length[which(boot_summary$mean_fem_prob_hd > 0.95 & boot_summary$CI_width_HD < 0.05)])
summary(boot_summary$mean_R.HD[which(boot_summary$mean_fem_prob_hd > 0.95 & boot_summary$CI_width_HD < 0.05)])

#mature males:
summary(boot_summary$mean_length[which(boot_summary$mean_fem_prob_hd < 0.05 & boot_summary$CI_width_HD < 0.05)])
summary(boot_summary$mean_R.HD[which(boot_summary$mean_fem_prob_hd < 0.05 & boot_summary$CI_width_HD < 0.05)])

length(boot_summary$mean_R.HD[which(boot_summary$mean_fem_prob_hd < 0.05 & boot_summary$CI_width_HD < 0.05)])

