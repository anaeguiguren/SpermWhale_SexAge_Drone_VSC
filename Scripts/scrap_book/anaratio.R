# Simmulating whales -----
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)



#dtable <- read.csv("Output_Data/id.mean.measures.csv", header = T)
dtable <- read.csv("Output_Data/id.morpho.output.clean.csv", header = T)


# 2. filter data ----
testtab <- dtable %>%
  select(Length = TL.m, Ratio = ratio.HF, ID, suckled_ever, suckling_ever)%>%
  filter(!is.na(Ratio))

#3. optimal curve -----


hf_mod <-optim.sex(data = testtab, 
          pard0 = c(fr = 0.33, fmax = 0.26, mr = 0.2, mmax = 0.49), 
          chm = 6)


#4. probability of being a female -----
testtab$P_fem<-f_probs(params = hf_mod$params, data = testtab)




params <- hf_mod$params

#5. average by Individual -----

pooled <- testtab %>%
  group_by(ID) %>%
  summarize(mean_TL = mean(Length), sd_TL = sd(Length),
            mean_Ratio = mean(Ratio), sd_Ratio = sd(Ratio),
            mean_p_F = mean(P_fem), 
            suckled_ever = first(suckled_ever))

#6. Plots ------
#~~~~a. average points widh sd



tlm = seq(3.6, 17, by = 0.2) # male length range
tlf = seq(3.6, 12, by = 0.2) #female length range

# Plotting -----

#~~~~a. average points widh sd------
ggplot(pooled, aes(x = mean_TL, y = mean_Ratio, color = mean_p_F, shape = suckled_ever)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_Ratio-sd_Ratio, ymax = mean_Ratio+ sd_Ratio))+
  geom_errorbarh(aes(xmin = mean_TL-sd_TL, xmax = mean_TL + sd_TL))+
  scale_color_gradient(low = "blue", high = "red", name = "Posterior Pr (female)") +
  theme_minimal()



#~~~~~b. average points with sd and underlying curves----

curve_f <- function(x) {
  female_curve(x, params[1], params[2])
}
curve_m <- function(x) {
  male_curve(x, params[1], params[2], params[3], params[4], chm)
}

#predicted curves
curve_df <- tibble(
  Length = c(tlf, tlm),
  Curve = c(rep("Female", length(tlf)), rep("Male", length(tlm))),
  Ratio = c(curve_f(tlf), curve_m(tlm))
)


#get a subset of suckled whales:

se <-pooled %>%
  filter(suckled_ever==T)

ggplot() +
  # Use color for curve lines (discrete)
  geom_line(data = curve_df, aes(x = Length, y = Ratio, color = Curve), linewidth=1) +
  
  # Use fill or color gradient for points
  geom_point(data = pooled, aes(x = mean_TL, y = mean_Ratio, fill = mean_p_F), size =3,shape = 21) +
  #geom_point(data = se, aes(x = mean_TL, y = mean_Ratio, fill = mean_p_F), shape = 22, size = 4)+
  
  scale_color_manual(values =c("#FF7D4C", "#003C8D")) +
  scale_fill_gradientn(colors= c("#003C8D", "#CC3A91","#FF7D4C")) +
  
  theme_classic()

#~~~~~c. probability as a function of size -------

ggplot(pooled,(aes(x = mean_TL ,y = mean_p_F)))+
  geom_point()+
  geom_hline(yintercept = 0.95, linetype = "dashed")+
  geom_hline(yintercept = 0.05, linetype = "dashed")



ggplot(pooled,(aes(x = mean_Ratio ,y = mean_p_F)))+
  geom_point()+
  geom_hline(yintercept = 0.95, linetype = "dashed")+
  geom_hline(yintercept = 0.05, linetype = "dashed")




# --- Write results back ---
dtable$Pr_F <- NA
dtable$Pr_F[!is.na(dtable$sd_ratio.HF)] <- testtab$Pr_female
write_csv(dtable, "Ratio_to_sex_R_version.csv")



#compare to matlab_outputs:
m.rat <-read.csv("C:/Users/balae/Documents/MATLAB/SpermWhale_age_sex/Ratio_to_sex.csv", header = T)
hist(m.rat$Pr_F- dtable$Pr_F, breaks = 15)
#super super close when initial parameters are informed 