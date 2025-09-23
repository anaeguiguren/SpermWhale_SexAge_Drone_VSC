
rm(list = ls())

load("bootstrapped_estimates.RData")
source("Scripts/functions.R")
dat.mean <- read.csv("Data/id.morpho.output.mean_prior_check.csv")

d <- left_join(dat.mean, dat, by = "ID")

#1. Prepare data -----

# load males

mad<-read.csv("Data/males_david.csv", header = T)
mad


#make dataframe with original whales

dat <- boot_summary%>%
  select(ID,Length = mean_length, 
         R = mean_R.HF,
         mean_fem_prob_hf
         )

#make new dataframe combining both

dat.m <- data.frame(
  ID = c(boot_summary$ID, mad$VideoFile),
  Length = c(boot_summary$mean_length, mad$TL.m),
  Ratio = c(boot_summary$mean_R.HF, mad$R.HF.m)
)

#2. Estimate Curve Parameters  -----
#based on original dataset

hf.temp <- optim_sex(dat,
                     chm = 6, 
                     pard0 =  c(nish$Value[3],
                                nish$Value[1],
                                nish$Value[4],
                                nish$Value[2]), weighted = FALSE)


#estimate f_prob (original dataset)

dat$fem_prob_hf <- f_probs(hf.temp$params, data = dat %>% mutate(Ratio = R))



#3. Estimate probatilities -----
#based on combined dataset







# 1. estimate residuals (everyone + males)-----
# based on median params?
fr_med = median(hf_params_df$fr)
fmax_med = median(hf_params_df$mmax)
mr_med = median(hf_params_df$mr)
mmax_med = median(hf_params_df$mmax)


  chm <- 6
  fr <- fr_med
  fmax <- fmax_med
  mr <- mr_med
  mmax <- mmax_med
  
  preds_f <- fem_curve(dat$Length, fr, fmax)
  
  
  
  preds_m <- mal_curve(dat$Length, fr, fmax, mr, mmax, chm)
  
  resid_f <- (dat$Ratio - preds_f)^2 #female squared residuals
  resid_m <- (dat$Ratio - preds_m)^2 # male squared residuals
  residuals <- pmin(resid_f, resid_m) #returns the minimum of each curve for each data point
  

  likes <- cbind(resid_f, resid_m)
  ss <- sum(residuals)

# 2. estimate p(F)-----


#f_probs <- function(params, data, chm = 6, weighted = FALSE) {
  #res <- sumsq(params, data, chm)
  likes <- exp(-likes / (2 * ss / (nrow(likes) - 1)))
  post_probs <- likes[, 1] / rowSums(likes)
  return(post_probs)

#}


