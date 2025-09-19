# Growth Curve Parameter Optimization
# Load necessary libraries and functions
library(readxl)
source("Scripts/functions.R")
set.seed(1234567)

# 0. Load Hal's formula results-----
hal<-read_excel("/Users/anacristinaeguigurenburneo/Documents/MATLAB/respermwhalemeasures/Checking_Ana_Hal.xlsx")
# 1. Load cleaned data ----
#mean metrics per individual:
dat <- read.csv("Data/Processed_Data/id_morpho_output_clean_processed_hf.csv") 

#nishiwaki parameters from tip of snout to center of eye
#nish <- read.csv("Data/nishiwaki_parameters.csv", header = T)


#write.csv(dat_HF, "Data/Processed_Data/id_morpho_output_clean_processed_hf.csv")

# 2. use Hal's output parameters to estimate expected R based on TL-----

pardeal=c(0.6462,    0.3023,    0.0003,  133.8194)  #unweighted outputs from Matlab

#~~~~a. females----
fem_curve <- function(length, fr, fmax) {
  fmax * exp(fr * length) / (1 + exp(fr * length))
}

# estimate
dat$fem_R<-fem_curve(length = dat$Length, fr = pardeal[1], fmax = pardeal[2])


#compare
hist(dat$fem_R - hal$Restf)# No difference here!
max(dat$fem_R - hal$Restf)


#~~~~b. males -----
#fx
mal_curve_y <- function(length, fr, fmax, mr, mmax, chm){
  base <- fmax * exp(fr  * length) / (1 + exp(fr * length))
  
  offset <- (length > chm) * mmax * 
    (exp(mr * length) / (1 + exp(mr * length)) -
       exp(mr * chm) / (1 + exp(mr * chm)))
  Ratio <- base + offset
  return(Ratio)
}
#estimate
dat$mal_R<- mal_curve_y(length = dat$Length, 
          fr = pardeal[1],
          fmax = pardeal[2], 
          mr = pardeal[3],
          mmax = pardeal[4], 
          chm = 6)# something's up here!!!


hist(dat$mal_R - hal$Restm)# something's up here!!!
max(dat$mal_R - hal$Restm)# 0.006 difference (?)


#alternate formula?


mal_curve_x <- function(length, fr, fmax, mr, mmax, chm){
  fmax * exp(fr * pmin(length, chm)) / (1 + exp(fr * pmin(length, chm))) +
    (length > chm) * mmax *
    (
      exp(mr * length) / (1 + exp(mr * length)) -
        exp(mr * chm) / (1 + exp(mr * chm))
    )
}

dat$mal_R_x<- mal_curve_x(length = dat$Length, 
                             fr = pardeal[1],
                             fmax = pardeal[2], 
                             mr = pardeal[3],
                             mmax = pardeal[4], 
                             chm = 6)# something's up here!!!


hist(dat$mal_R_x - hal$Restm)# better!!!
max(dat$mal_R_x - hal$Restm)# 


#3. Compute optimal parameters using updated male curve ------

# add new male curve formula to sumsq
sumsq <- function(params, data, chm , weighted = FALSE){
  fr <- params[1]
  fmax <- params[2]
  mr <- params[3]
  mmax <- params[4]
  
  preds_f <- fem_curve(data$Length, fr, fmax)
  preds_m <- mal_curve_x(data$Length, fr, fmax, mr, mmax, chm)
  
  resid_f <- (data$Ratio - preds_f)^2 #female squared residuals
  resid_m <- (data$Ratio - preds_m)^2 # male squared residuals
  residuals <- pmin(resid_f, resid_m) #returns the minimum of each curve for each data point
  
  
  #when weighted by SD:
  if(weighted){
    ss <- sum(residuals/data$SD_Ratio) #sum of squares divided by sd
    return(ss)
  }else{
    likes <- cbind(resid_f, resid_m)
    ss <- sum(residuals)
    return(list(ss = ss, likes = likes))
  }
}

#update optim sex function
optim_sex <- function(data, chm,  pard0, weighted = FALSE){
  objfun <- function(p){ #this is the thing we want to minimize (optimize)
    if(weighted) {
      sumsq(p, data , chm, TRUE)
    }else{
      sumsq(p, data , chm, FALSE)$ss
    }
  }
  
  fit <- optim(pard0, objfun, control= list(maxit = 205000))
  params <- fit$par
  ss <- fit$value
  
  cat(ifelse(weighted, "Weighted SS", "Unweighted SS"), "\n")
  cat(sprintf("Sum of squares: %6.4f\n", ss)) #what are these percentages about?
  cat(sprintf("Fitted parameters: fr = %5.2f, fmax = %5.2f, mr = %5.2f, mmax = %5.2f\n",
              params[1], params[2], params[3], params[4]))
  
  list(params = params, ss = ss, fit = fit)
  
  
}

# give right names to dataframe

dat <- dat %>%
  mutate(Ratio = R.HF, R_sd = sd_ratio.HF)

#~~~~a. use Hal's initial parameters first----
#pard0=[0.33 0.26 0.2 0.49];

hf_mod <-optim_sex(data = dat, 
                   pard0 = c(fr = 0.33, fmax = 0.26, mr = 0.2, mmax = 0.49), 
                   chm = 6)
hf_params<- hf_mod$params#parameter values for males are slightly different (although may not be a big deal?)


# estimate p_fem using new formulas
dat$P_fem<-f_probs(params = hf_params, data = dat)

view(dat)




write.csv(dat, "Data/Processed_Data/comparing_hal/out_pfem_halparams.csv")

