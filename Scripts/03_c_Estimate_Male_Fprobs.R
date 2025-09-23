
rm(list = ls())

load("bootstrapped_estimates_plus_mean.RData")
source("Scripts/functions.R")

dat <- dat_HF



#1. Prepare data -----

# load males

mad<-read.csv("Data/males_david.csv", header = T)
mad


#make dataframe with original whales - bootstrap mean measures, singl p_fem estimates


dat <- dat %>%
  select(ID, Length = Length, 
         R = R.HF,
         P_fem_HD, P_fem_HF
         )

#make new dataframe combining both

dat.m <- data.frame(
  ID = c(dat$ID, mad$VideoFile),
  Length = c(dat$Length, mad$TL.m),
  Ratio = c(dat$R, mad$R.HF.m)
)

#2. call parameters from model fitting (HF only)  -----

hf_params


#3. Estimate probatilities -----
#based on combined dataset

dat.m$P_fem <- f_probs(params = hf_params, data = dat.m)

mad<-mad %>%
  mutate(Ratio = R.HF.m, 
         Length = TL.m, ID = VideoFile)
f_probs(params = hf_params, data = mad)#also super low
#4. save dataset -----

write.csv(dat.m, "Data/Processed_Data/output_p_fem_full_and_males.csv")
