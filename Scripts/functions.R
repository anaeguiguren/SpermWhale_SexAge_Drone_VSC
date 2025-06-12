# Custom-made functions for the project

#0. Load packages ----
library(tidyverse) # for data manipulation and visualization
library(stringr) # for string manipulation
library(ROCR) # estimates true positive and false positive rates 
library(PresenceAbsence) # builds confusion matrix

#1. Optimizing growth curve parameters ----

#~~~a. Define female and male curve shapes ----
fem_curve <- function(length, fr, fmax) {
  fmax * exp(fr * length) / (1 + exp(fr * length))
}

mal_curve <- function(length, fr, fmax, mr, mmax, chm){
  base <- fmax * exp(fr  * length) / (1 + exp(fr * length))

  offset <- (length > chm) * mmax * 
    (exp(mr * length) / (1 + exp(mr * length)) -
       exp(mr * chm) / (1 + exp(mr * chm)))
  Ratio <- base + offset
  return(Ratio)
}

#~~~b. Estimate sum of squares ----

sumsq <- function(params, data, chm, weighted = FALSE){
  fr <- params[1]
  fmax <- params[2]
  mr <- params[3]
  mmax <- params[4]
  
  preds_f <- fem_curve(data$Length, fr, fmax)
  preds_m <- mal_curve(data$Length, fr, fmax, mr, mmax, chm)
  
  resid_f <- (data$Ratio - preds_f)^2 #female sum of squares
  resid_m <- (data$Ratio - preds_m)^2 # male sum of squares
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


#~~~c. Fit parameters using optim ----
optim_sex <- function(data, chm, pard0, weighted = FALSE){
  objfun <- function(p){ #this is the thing we want to minimize (optimize)
    if(weighted) {
      sumsq(p, data, chm, TRUE)
    }else{
      sumsq(p, data, chm, FALSE)$ss
    }
  }
  
  fit <- optim(pard0, objfun, method = "Nelder-Mead" )
  params <- fit$par
  ss <- fit$value
  
  cat(ifelse(weighted, "Weighted SS", "Unweighted SS"), "\n")
  cat(sprintf("Sum of squares: %6.4f\n", ss)) #what are these percentages about?
  cat(sprintf("Fitted parameters: fr = %5.2f, fmax = %5.2f, mr = %5.2f, mmax = %5.2f\n",
              params[1], params[2], params[3], params[4]))
  
  list(params = params, ss = ss, fit = fit)
  
  
}




#~~~d. Estimate posterior probability of being female ----

f_probs <- function(params, data, chm = 6, weighted = FALSE) {
  res <- sumsq(params, data, chm)
  likes <- exp(-res$likes / (2 * res$ss / (nrow(res$likes) - 1)))
  post_probs <- likes[, 1] / rowSums(likes)
  return(post_probs)
}

#~~~e. compute classification performance -----
model_perf <- function(bin_sex, fem_probs){
  # find threshlold that maximizes area under the curve
  
  pred <- prediction(predictions = fem_probs, labels = bin_sex)
  
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  y <- as.data.frame(perf@y.values)
  x <- as.data.frame(perf@x.values)
  
  fi <- atan(y/x) - pi/4 # to calculate the angle between the 45? line and the line joining the origin with the point (x;y) on the ROC curve
  L  <- sqrt(x^2+y^2) # to calculate the length of the line joining the origin to the point (x;y) on the ROC curve
  d  <- L*sin(fi) 
  names(d)<-"vals"
  d$vals[which(d$vals=="NaN")]<-0
  maxd<-max(d$vals)
  maxpos<-max(which(d$vals==maxd))
  
  
  alpha <-as.data.frame(perf@alpha.values)
  thresh<-alpha[maxpos,]
  
  
  #build confusion matrix 
  datlength<-length(fem_probs)
  DATA           <- matrix(0,datlength,3)
  DATA           <- as.data.frame(DATA)
  names(DATA)    <-c("plotID", "Observed", "Predicted")
  DATA$plotID    <-1:datlength[1]
  DATA$Observed  <- bin_sex
  DATA$Predicted <- fem_probs
  conmat<-cmx(DATA, threshold = thresh)#this is the cutoff
  tp <- conmat[1,1]/ (conmat[1,1]+conmat[2,1]) # proportion of true females identified correctly
  tn <- conmat[2,2]/(conmat[1,2]+conmat[2,2]) # proportion of true males identified correctly
  
  return(list(threshold = thresh, 
              conmat = conmat, 
              true.pos = tp, 
              true.neg = tn))
}


#~~~f. get classification accuracy accross size bins -----
perf_bins <- function(data, length_bins = seq(4,17, by =1), threshold) {
  data$length_bin <- cut(data$Length, breaks = length_bins,
                         labels = paste(length_bins[-length(length_bins)],
                         length_bins[-1],
                         sep = "-"),
                         include.lowest = TRUE)
  #assign sex to classes
  data$Sex_bin_pred <- ifelse(data$Pr_female >= threshold, 1, 0)
  data$Sex_bin_wrong <- abs(data$Sex_bin - data$Sex_bin_pred)

  #summarize by bin

  bin_summary <- data %>%
   group_by(length_bin) %>%
   summarise(sum_wrong = sum(Sex_bin_wrong),
             prop_right = 1 - sum_wrong/n())
  
  return(bin_summary)
}


#~~~~g. generating data with noise and measurement errors -----


generate_repeated_measures <- function(true_ratio, n_measures, sd) {
  rnorm(n_measures, mean = true_ratio, sd = sd)
}

# Function to generate one dataset with specified uncertainty
generate_dataset <- function(uncertainty_sd, true_params) {
  # Generate lengths
  x_F <- rnorm(n_whales, mean = mean.L.F, sd = (max.L.F - mean.L.F) / 3) %>%
    pmin(max.L.F) %>%
    pmax(min.L)
  
  x_M <- runif(n_whales, min = min.L, max = max.L.M)
  
  # Generate true ratios with biological variation
  true_ratio_F <- with(true_params,
                       fmax * exp(fr * x_F) / (1 + exp(fr * x_F)) + 
                         rnorm(n_whales, mean = 0, sd = biological_sd)
  )
  
  true_ratio_M <- with(true_params, {
    base <- fmax * exp(fr * x_M) / (1 + exp(fr * x_M))
    
    offset <- (x_M > chm) * mmax * (
      exp(mr * x_M) / (1 + exp(mr * x_M)) -
        exp(mr * chm) / (1 + exp(mr * chm))
    )
    base + offset + rnorm(n_whales, mean = 0, sd = biological_sd)
  })
  
  # Generate repeated measurements with measurement error
  measurements_F <- map2_dfr(true_ratio_F, seq_along(true_ratio_F), 
                             ~data.frame(
                               ID = paste0("F", .y),
                               Length = x_F[.y],
                               true_ratio = .x,
                               Ratio = generate_repeated_measures(.x, n_measurements, uncertainty_sd),
                               measure_num = 1:n_measurements,
                               Sex = "F"
                             )
  )
  
  measurements_M <- map2_dfr(true_ratio_M, seq_along(true_ratio_M),
                             ~data.frame(
                               ID = paste0("M", .y),
                               Length = x_M[.y],
                               true_ratio = .x,
                               Ratio = generate_repeated_measures(.x, n_measurements, uncertainty_sd),
                               measure_num = 1:n_measurements,
                               Sex = "M"
                             )
  )
  
  # Combine and calculate measurement standard deviations
  dataset <- rbind(measurements_F, measurements_M) %>%
    group_by(ID) %>%
    mutate(sd_R = sd(Ratio))%>%
    ungroup()
  
  
  return(dataset)
}


