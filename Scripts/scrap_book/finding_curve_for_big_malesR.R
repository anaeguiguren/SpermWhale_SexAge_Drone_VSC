#check female curve estimates
min.L <- 4 # length at birth
max.L.F <- 16.5 # max length females
max.L.M <- 16.5 #Max male length

#length sequences:
x_F <- seq(min.L, max.L.F, by = 0.2)
x_M <- seq(6, max.L.M, by = 0.2)

length= x_F#should be using the median, not the mean. 
fr = 0.71
fmax = mean_hf_params$fmax

fmax * exp(fr * length) / (1 + exp(fr * length))







mal_curve <- function(length, fr, fmax, mr, mmax, chm){
  base <- fmax * exp(fr  * length) / (1 + exp(fr * length))
  
  offset <- (length > chm) * mmax * 
    (exp(mr * length) / (1 + exp(mr * length)) -
       exp(mr * chm) / (1 + exp(mr * chm)))
  Ratio <- base + offset
  return(Ratio)
}

