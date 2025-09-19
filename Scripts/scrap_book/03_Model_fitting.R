# Growth Curve Parameter Optimization
# Load necessary libraries and functions
source("Scripts/functions.R")
set.seed(1234567)

# 1. Load cleaned data ----
#mean metrics per individual:
clean_data <- read.csv("Data/Processed_Data/id_morpho_output_clean_processed.csv") 

#nishiwaki parameters from tip of snout to center of eye
nish <- read.csv("Data/nishiwaki_parameters.csv", header = T)



# 2. Subset datasets ----
# head to dorsal fin: 
dat_HD <- clean_data %>%
  select(ID, Length = mean_TL, Length_SD = sd_TL,
         Ratio = mean_ratio.HD, R_sd = sd_ratio.HD,
         suckled_ever, suckling_ever, n_photos) %>%
  filter(n_photos > 2 & !is.na(R_sd))

dim(dat_HD)

dat_HF <- clean_data %>%
  select(ID, Length = mean_TL, Length_SD = sd_TL,
         Ratio = mean_ratio.HF, R_sd = sd_ratio.HF,
         suckled_ever, suckling_ever, n_photos) %>%
  filter(n_photos > 2 & !is.na(R_sd))
dim(dat_HF)

#check:

# 3. HD model ----

#growth curves
hd_mod <-optim_sex(data = dat_HD, 
                   pard0 = c(fr = nish[3,2], fmax = nish[1,2], mr = nish[4,2], mmax = nish[2,2]), 
                   chm = 6)

hd_params <- hd_mod$params

#posterior probabilities
dat_HD$P_fem<-f_probs(params = hd_params, data = dat_HD)


# 4. HF model ----

#growth curves
hf_mod <-optim_sex(data = dat_HF, 
                   pard0 = c(fr = nish[3,2], fmax = nish[1,2], mr = nish[4,2], mmax = nish[2,2]), 
                   chm = 6)

hf_params <- hf_mod$params
#posterior probabilities
dat_HF$P_fem<-f_probs(params = hf_params, data = dat_HF)

# 5. Visualize ------

tlm = seq(3.6, 17, by = 0.2) # male length range
tlf = seq(3.6, 12, by = 0.2) #female length range

#~~~~a. HD----

#predicted curves
curve_df_HD <- tibble(
  Length = c(tlf, tlm),
  Curve = c(rep("Female", length(tlf)), rep("Male", length(tlm))),
  Ratio = c(fem_curve(tlf, fr = hd_params[1], fmax = hd_params[2]),
            mal_curve(tlm, fr = hd_params[1], fmax = hd_params[2],
                      mr = hd_params[3], mmax = hd_params[4], chm = 6))
)


#get a subset of suckled whales:

se_hd <- dat_HD %>%
  filter(suckled_ever==T)

p1 <- ggplot() +
  # Use color for curve lines (discrete)
  geom_line(data = curve_df_HD, aes(x = Length, y = Ratio, color = Curve), linewidth=1) +
  
  # Use fill or color gradient for points
  geom_point(data = dat_HD, aes(x = Length, y = Ratio, fill = P_fem), size =3,shape = 21, alpha = 0.7) +
  geom_point(data = se_hd, aes(x = Length, y = Ratio, fill = P_fem), shape = 22, size = 4)+
  geom_text(data = se_hd, aes(x = Length, y = Ratio,
                              label = gsub("gal2023_0", "", tolower(se_hd$ID))), hjust = 0.5, vjust = 0.5, size = 2)+
  scale_color_manual(values =c("darkcyan", "darkorange")) +
  scale_fill_gradientn(colors= c("darkorange","darkcyan")) +
  
  theme_classic()+
  theme(legend.position = "null")+
  xlab("Length (m)") + ylab("Ratio (HD)")




#~~~~b. HF----
#predicted curves
curve_df_HF<- tibble(
  Length = c(tlf, tlm),
  Curve = c(rep("Female", length(tlf)), rep("Male", length(tlm))),
  Ratio = c(fem_curve(tlf, fr = hf_params[1], fmax = hf_params[2]),
            mal_curve(tlm, fr = hf_params[1], fmax = hf_params[2],
                      mr = hf_params[3], mmax = hf_params[4], chm = 6))
)


#get a subset of suckled whales:

se_hf <- dat_HF %>%
  filter(suckled_ever==T)

p2 <- ggplot() +
  # Use color for curve lines (discrete)
  geom_line(data = curve_df_HF, aes(x = Length, y = Ratio, color = Curve), linewidth=1) +
  
  # Use fill or color gradient for points
  geom_point(data = dat_HF, aes(x = Length, y = Ratio, fill = P_fem), size =3, shape = 21, alpha = 0.7) +
  geom_point(data = se_hf, aes(x = Length, y = Ratio, fill = P_fem), shape = 22, size = 4)+
  geom_text(data = se_hf, aes(x = Length, y = Ratio,
                              label = gsub("gal2023_0", "", tolower(se_hf$ID))), hjust = 0.5, vjust = 0.5, size = 2)+
  scale_color_manual(values =c("darkcyan", "darkorange")) +
  scale_fill_gradientn(colors= c("darkorange","darkcyan")) +
  labs(fill = "P(F)", colour = "")+
  xlab("Length (m)") + ylab("Ratio (HF)")+
  theme_classic()

#~~~~c. save -----
library(patchwork)
combined_plot <- p1 + p2


ggsave("Figures/modelled_curves_hf_hd_raw.png",
       combined_plot, width = 13, height = 6)



