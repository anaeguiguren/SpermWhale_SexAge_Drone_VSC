
# Checking the linear version of male model

rm(list = ls())

source("Scripts/functions.R")

set.seed(1234567)

# 1. Load data ----
#only rows that could be assigned individual IDs
clean_data <- read.csv("Data/Processed_Data/id_unpooled_clean_processed.csv") 
nish <- read.csv("Data/Processed_Data/nishiwaki_parameters.csv")

# subset relevant columns
dat<- clean_data %>%
  select(ID, Length = TL.m, 
         R.HD = ratio.HD, R.HF = ratio.HF, 
         suckled_ever, suckling_ever)

# make a data frame for HF ratio:
dat_HF <- dat %>%
  group_by(ID) %>%
  filter(sum(!is.na(R.HF)) >= 3)  %>%
  ungroup() %>%
  filter(!is.na(R.HF))


tmp.dat <- dat_HF %>%
  group_by(ID) %>%
  slice_sample(n = 1)


#2. find optimal curve -----
#exponential (HD)
hd.temp <- optim_sex_b(data = tmp.dat %>% mutate(Ratio = R.HD),
                     chm = 6, 
                     pard0 = c(fmax = nish$Value[1], 
                               fr = nish$Value[3], 
                               mmax = nish$Value[2], 
                               mr = nish$Value[4]), 
                     exponential_male_growth = TRUE,
                     weighted = FALSE)

hd.temp$params
hd.temp$ss



hd.temp.lin <- optim_sex_b(data = tmp.dat %>% mutate(Ratio = R.HD),
                         chm = 6, 
                         pard0 = c(fmax = nish$Value[1], 
                                   fr = nish$Value[3], 
                                   mr_l = nish$Value[5]), 
                         exponential_male_grow = FALSE, 
                         weighted = FALSE)

#params
hd.temp.lin$ss
hd.temp.lin$params

#3. make curve:-----

length_seq <- seq(4, 17, by = 0.1)

lin.fem <- fem_curve(length = length_seq, 
                     fr = hd.temp.lin$params["fr"], 
                     fmax = hd.temp.lin$params["fmax"])

lin.mal <- mal_curve_l(length = length_seq, 
                     fr = hd.temp.lin$params["fr"], 
                     fmax = hd.temp.lin$params["fmax"], 
                     mr_l = hd.temp.lin$params["mr_l"], 
                     chm = 6)

fem_line <- data.frame(Length = length_seq, 
                       Ratio = lin.fem, 
                       Sex = "female")

# estimate the derivative for this function (check that AI function
# is correct)

fem_exp <- expression(fmax * exp(fr * length) / (1 + exp(fr * length)))

deriv_fem <- function(fmax, fr, length){
  num = fmax * fr * exp(fr*length)
  den = (1 + exp(fr * length))^2
  num/den
}



# Create a derivative function

# This creates a function you can call
slope <- deriv_fem(fmax = unname(hd.temp.lin$params['fmax']),
           fr = unname(hd.temp.lin$params['fr']),
          length = 6)


deriv_fem(fr = 213, fmax = 1766, length = 6)

y0 <- fem_curve(length = 6, 
                fr = unname(hd.temp.lin$params["fr"]), 
                fmax = unname(hd.temp.lin$params["fmax"]))


intercept <- y0 - slope * 6

# add line to plot
ggplot(data = fem_line, aes(x = Length, y = Ratio)) +
  geom_line()+
  geom_abline(slope = slope, intercept = intercept, 
              color = "red", linetype = "dashed") +
  geom_point(aes(x = 6, y = y0), color = "red", size = 3) +
  labs(x = "Length", y = "Value")+
  ylim(0.62, 0.73)

# this is ok...but maybe too high?

# 4. try a linear curve that goes from NR(chm) to NR(18) (i.e., fmax)----
#nr at length = chm
y_chm <- fem_curve(length = 6, 
                fr = unname(hd.temp.lin$params["fr"]), 
                fmax = unname(hd.temp.lin$params["fmax"]))

#nr at length = 18m ~ fmax (identical!)
y_max <- fem_curve(length = 18, 
                   fr = unname(hd.temp.lin$params["fr"]), 
                   fmax = unname(hd.temp.lin$params["fmax"]))


# estimate a minimal slope
slope_min <- 2 *  (y_max - y_chm) / (18 - 6)



y_intercept <- y_chm - (slope_min * 6)




# add this line to plot:
# add line to plot
ggplot(data = fem_line, aes(x = Length, y = Ratio)) +
  geom_line()+
  geom_abline(slope = slope, intercept = intercept, 
              color = "red", linetype = "dashed") +
  geom_point(aes(x = 6, y = y0), color = "red", size = 3) +
  geom_abline(slope = slope_min, intercept = y_intercept, 
              color = "blue", linetype = "dashed")+
  labs(x = "Length", y = "Value")+
  ylim(0.62, 0.73)




  


mal_line <- data.frame(Length = length_seq, 
                       Ratio = lin.mal, 
                       Sex = "male")


lin.dat <- data.frame(
  Length =  rep(length_seq, times = 2),
  Ratio = c(lin.fem, lin.mal),
  Sex = c(rep("female", length.out = length(length_seq)),
          rep("male", length.out = length(length_seq)))
)





ggplot(tmp.dat %>% mutate(Ratio = R.HD), 
       aes(x = Length, y = Ratio))+
  geom_point(alpha = 0.7, size = 2)+
  geom_line(inherit.aes = F, 
            data = lin.dat, 
            aes(x = Length, y = Ratio, colour = Sex))+
  geom_abline(slope = slope, intercept = intercept, 
              color = "red", linetype = "dashed") 
  





f_probs <- function(params, data, chm = 6, exponential_male_growth ,weighted = FALSE) {
  res <- sumsq(params = params, data = data, chm = chm, exponential_male_growth = exponential_male_growth, weighted = weighted)
  likes <- exp(-res$likes / (2 * res$ss / (nrow(res$likes) - 1)))
  post_probs <- likes[, 1] / rowSums(likes)
  return(post_probs)
}


f_probs_exp<- f_probs(params = hd.temp$params, 
        data = tmp.dat %>% mutate(Ratio = R.HD),
        chm = 6, 
        exponential_male_growth = TRUE, 
        weighted = FALSE)

f_probs_lin<- f_probs(params = hd.temp.lin$params, 
                      data = tmp.dat %>% mutate(Ratio = R.HD),
                      chm = 6, 
                      exponential_male_growth = FALSE, 
                      weighted = FALSE)

plot(f_probs_exp, f_probs_lin)# pretty much the same!


tmp.dat$f_probs_e <- f_probs_exp
tmp.dat$f_probs_l <- f_probs_lin

ggplot(tmp.dat %>% mutate(Ratio = R.HD), 
       aes(x = Length, y = Ratio))+
  geom_point(aes(colour = f_probs_l),alpha = 0.7, size = 2)+
  geom_line(inherit.aes = F, 
            data = fem_line, 
            aes(x = Length, y = Ratio), colour = "lightblue")+
  geom_line(inherit.aes = F, 
            data = mal_line,  
            aes(x = Length , y = Ratio), colour = "darkblue")+
  theme_classic()


