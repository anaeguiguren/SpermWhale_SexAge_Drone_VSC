# Custom-made functions for the project

#0. Load packages ----
library(tidyverse) # for data manipulation and visualization
library(stringr) # for string manipulation

#1. Optimizing growth curve parameters ----

#~~~a. Define female and male curve shapes ----
fem_curve <- function(length, r_F = params$Value[3],
                      max_R_F = params$Value[1]) {
  max_R_F * exp(r_F * length) / (1 + exp(r_F * length))
}

mal_curve <- function(length, r_F = params$Value[3],
                      max_R_F = params$Value[1],
                      r_M = params$Value[4], 
                      chm = 6, max_R_M = params$Value[2]){
  base <- max_R_F * exp(r_F  * length) / (1 + exp(r_F * length))

  offset <- (length > chm) * max_R_M * 
    (exp(r_M * length) / (1 + exp(r_M * length)) -
       exp(r_M * chm) / (1 + exp(r_M * chm)))
  Ratio <- base + offset
  return(Ratio)
}

#~~~b. Estimate sum of squares ----
sumsq <- function(params, data, chm, weighted = FALSE){
  r_F <- params$Value[3]
  max_R_F <- params$Value[1]
  r_M <- params$Value[4]
  max_R_M <- params$Value[2]
  #estimate predictions based on parameters and data
  preds_f <- fem_curve(data$Length)
  preds_m <- mal_curve(data$Length)
  #calculate residuals
  resid_f <- (data$Ratio - preds_f)^2 #female sum of squares
  resid_m <- (data$Ratio - preds_m)^2 # male sum of squares
  #take the minimum of the two residuals
  residuals <- pmin(resid_f, resid_m)
  #when weighted by SD:
  if (weighted) {
    ss <- sum(residuals / data$SD_Ratio) #sum of squares divided by sd
    return(ss)
  } else {
    likes <- cbind(resid_f, resid_m)
    ss <- sum(residuals)
    return(list(ss = ss, likes = likes))
  }
}

#~~~c. Fit parameters using optim ----
optim_sex <- function(data,
                      pard0 = c(fr = 0.2, fmax = 0.2, mr = 0.2, mmax = 0.2),
                      chm = 6, weighted = FALSE) {
  #this is the thing we want to minimize (optimize)
  objfun <- function(p) {
    if (weighted) {
      sumsq(p, data, chm, TRUE)
    } else {
      #unweighted
      sumsq(p, data, chm, FALSE)$ss
    }
  }
  #optimizing parameters
  fit <- optim(pard0, objfun, method = "Nelder-Mead")
  params <- fit$par
  ss <- fit$value
  # Print results
  cat(ifelse(weighted, "Weighted SS", "Unweighted SS"), "\n")
  cat(sprintf("Sum of squares: %6.4f\n", ss)) #what are these percentages about?
  cat(sprintf("Fitted parameters: fr = %5.2f,
   fmax = %5.2f, mr = %5.2f, mmax = %5.2f\n",
              params[1], params[2], params[3], params[4]))

  out <- list(fit = fit, params =  params, ss = ss)
  return(out)
}

#~~~d. Estimate posterior probability of being female ----
f_probs <- function(params, data, chm = 6, weighted = FALSE) {
  res <- sumsq(params, data, chm)
  likes <- exp(-res$likes / (2 * res$ss / (nrow(res$likes) - 1)))
  post_probs <- likes[, 1] / rowSums(likes)
  return(post_probs)
}