
dat = tmp.dat %>% mutate(Ratio = R.HD)


ggplot(dat, 
       aes(x = Length, y = Ratio))+
  geom_point(alpha = 0.7, size = 2)




optim.hf.lin <- optim_sex(data = dat,
          chm = 6, 
          pard0 = c(fmax = nish$Value[1], 
                    fr = nish$Value[3], 
                    mr_l = nish$Value[5]), 
          exponential_male_growth = FALSE,
          weighted = FALSE)




#params
optim.hf.lin$params

#2. add curve:-----

length_seq <- seq(4, 17, by = 0.1)

lin.fem <- fem_curve(length = length_seq, 
                     fr = optim.hf.lin$params["fr"], 
                     fmax = optim.hf.lin$params["fmax"])

lin.mal <- mal_curve_l(length = length_seq, 
                     fr = optim.hf.lin$params["fr"], 
                     fmax = optim.hf.lin$params["fmax"], 
                     mr_l = optim.hf.lin$params["mr_l"], 
                     chm = 6)


lin.dat <- data.frame(
  Length =  rep(length_seq, times = 2),
  Ratio = c(lin.fem, lin.mal),
  Sex = c(rep("female", length.out = length(length_seq)),
          rep("male", length.out = length(length_seq)))
)





ggplot(dat, 
       aes(x = Length, y = Ratio))+
  geom_point(alpha = 0.7, size = 2)+
  geom_line(inherit.aes = F, 
            data = lin.dat, 
            aes(x = Length, y = Ratio, colour = Sex))
  





f_probs <- function(params, data, chm = 6, exponential_male_growth ,weighted = FALSE) {
  res <- sumsq(params = params, data = data, chm = chm, exponential_male_growth = exponential_male_growth, weighted = weighted)
  likes <- exp(-res$likes / (2 * res$ss / (nrow(res$likes) - 1)))
  post_probs <- likes[, 1] / rowSums(likes)
  return(post_probs)
}
