# Growth Curve Parameter Optimization
# Load necessary libraries and functions
rm(list = ls())

source("Scripts/functions.R")
set.seed(1234567)


# 1. Load cleaned data ----
#mean metrics per individual - based on boot means:
load("bootstrapped_estimates.RData")




#nishiwaki parameters from tip of snout to center of eye
nish <- read.csv("Data/nishiwaki_parameters.csv", header = T)


#subset relevant columns:
dat <- boot_summary %>%
  select(ID, Length = mean_length, 
         R.HD = mean_R.HD, 
         R.HF = mean_R.HF, 
         suckled_ever, suckling_ever)


# 2.rename dataset ----
# head to dorsal fin: 
# make a data frame for HF ratio:
dat_HF <- dat # already subset for no na's!

#check:

# 3. HD model ----

#growth curves
hd_mod <-optim_sex(data = dat_HF %>% mutate(Ratio = R.HD), 
                   pard0 = c(fr = nish[3,2], fmax = nish[1,2], mr = nish[4,2], mmax = nish[2,2]), 
                   chm = 6)

hd_params <- hd_mod$params

#posterior probabilities
dat_HF$P_fem_HD <- f_probs(params = hd_params, data = dat_HF%>% mutate(Ratio = R.HD))


# 4. HF model ----

#growth curves
hf_mod <-optim_sex(data = dat_HF %>% mutate(Ratio = R.HF), 
                   pard0 = c(fr = nish[3,2], fmax = nish[1,2], mr = nish[4,2], mmax = nish[2,2]), 
                   chm = 6)

hf_params <- hf_mod$params
#posterior probabilities
dat_HF$P_fem_HF<-f_probs(params = hf_params, data = dat_HF %>% mutate(Ratio = R.HF))

# 5. Visualize ------

tlm = seq(3.6, 17, by = 0.2) # male length range
tlf = seq(3.6, 12, by = 0.2) #female length range

#~~~~a. HD----

#predicted curves
curve_df_HD <- tibble(
  Length = c(tlf, tlm),
  Curve = c(rep("Female", length(tlf)), rep("Male", length(tlm))),
  R.HD = c(fem_curve(tlf, fr = hd_params[1], fmax = hd_params[2]),
            mal_curve(tlm, fr = hd_params[1], fmax = hd_params[2],
                      mr = hd_params[3], mmax = hd_params[4], chm = 6))
)


#get a subset of suckled whales:

se <- dat_HF %>%
  filter(suckled_ever==T)

p1 <- ggplot() +
  # Use color for curve lines (discrete)
  geom_line(data = curve_df_HD, aes(x = Length, y = R.HD, color = Curve), linewidth=1) +
  
  # Use fill or color gradient for points
  geom_point(data = dat_HF, aes(x = Length, y = R.HD, fill = P_fem_HD), size =3,shape = 21, alpha = 0.7) +
  geom_point(data = se, aes(x = Length, y = R.HD, fill = P_fem_HD), shape = 22, size = 4)+
  geom_text(data = se, aes(x = Length, y = R.HD,
                              label = gsub("gal2023_0", "", tolower(se$ID))), hjust = 0.5, vjust = 0.5, size = 2)+
  scale_color_manual(values =c("darkcyan", "darkorange")) +
  scale_fill_gradientn(colors= c("darkorange","darkcyan")) +
  
  theme_classic()+
  theme(legend.position = "null")+
  xlab("Length (m)") + ylab("Ratio (HD)")




p1
#~~~~b. HF----
#predicted curves
curve_df_HF<- tibble(
  Length = c(tlf, tlm),
  Curve = c(rep("Female", length(tlf)), rep("Male", length(tlm))),
  R.HF = c(fem_curve(tlf, fr = hf_params[1], fmax = hf_params[2]),
            mal_curve(tlm, fr = hf_params[1], fmax = hf_params[2],
                      mr = hf_params[3], mmax = hf_params[4], chm = 6))
)


#get a subset of suckled whales:


p2 <- ggplot() +
  # Use color for curve lines (discrete)
  geom_line(data = curve_df_HF, aes(x = Length, y = R.HF, color = Curve), linewidth=1) +
  
  # Use fill or color gradient for points
  geom_point(data = dat_HF, aes(x = Length, y = R.HF, fill = P_fem_HF), size =3, shape = 21, alpha = 0.7) +
  geom_point(data = se, aes(x = Length, y = R.HF, fill = P_fem_HF), shape = 22, size = 4)+
  geom_text(data = se, aes(x = Length, y = R.HF,
                              label = gsub("gal2023_0", "", tolower(ID))), hjust = 0.5, vjust = 0.5, size = 2)+
  scale_color_manual(values =c("darkcyan", "darkorange")) +
  scale_fill_gradientn(colors= c("darkorange","darkcyan")) +
  labs(fill = "P(F)", colour = "")+
  xlab("Length (m)") + ylab("Ratio (HF)")+
  theme_classic()

#~~~~c. save -----
library(patchwork)
combined_plot <- p1 + p2
combined_plot

ggsave("Figures/modelled_curves_hf_hd_raw.png",
       combined_plot, width = 13, height = 6)

head(dat_HF)


save.image("bootstrapped_estimates_plus_mean.RData")

