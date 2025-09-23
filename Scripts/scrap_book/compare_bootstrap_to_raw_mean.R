load("bootstrapped_estimates.RData")

#bootstrap outputs:
dat <- boot_summary%>%
  select(ID,Length = mean_length, 
         R = mean_R.HF,
         mean_fem_prob_hf
  )


#raw mean outputs:

dat.mean <- read.csv( "Data/Processed_Data/id_morpho_output_clean_processed.csv", header = T)


# join by ID:

d <- left_join(dat, dat.mean , by = "ID")

# explore differences in measurements:
ggplot(d, aes(x = Length, y= mean_TL))+
  geom_smooth(method = lm)+
  geom_point()


ggplot(d, aes(x = R, y= mean_ratio.HF))+
  geom_smooth(method = lm)+
  geom_point()

#they're pretty close!

#model fit using raw measurements:
dat_HF <- d %>%
  select(ID, Length = mean_TL,
         Ratio = mean_ratio.HF,
         suckled_ever, suckling_ever, n_photos) %>%
  filter(n_photos > 2)




hf_mod <-optim_sex(data = dat_HF, 
                   pard0 = c(fr = nish[3,2], fmax = nish[1,2], mr = nish[4,2], mmax = nish[2,2]), 
                   chm = 6)


#get female probs:
hf_params <- hf_mod$params

#posterior probabilities
dat_HF$P_fem<-f_probs(params = hf_params, data = dat_HF)

#nice

#re-join to boot params

c<-left_join(d, dat_HF, by = "ID")


#plot
ggplot(c, aes(x = mean_fem_prob_hf, y = P_fem))+
  geom_point(alpha = 0.5, size = 3)



# what happens if I get the probs from the bootstrapped means?
d_boot <- dat %>%
  select(Ratio = R, Length, ID)
hf_mod_boot <-optim_sex(data = d_boot, 
                   pard0 = c(fr = nish[3,2], fmax = nish[1,2], mr = nish[4,2], mmax = nish[2,2]), 
                   chm = 6)


#get female probs:
hf_params_boot <- hf_mod_boot$params

#posterior probabilities
d_boot$P_fem_boot<-f_probs(params = hf_params_boot, data = d_boot)

head(d_boot)

#compare bootstrapped p_fems, to general p_fems

m <- left_join(c, d_boot, by = "ID")


ggplot(data = m, aes(x = P_fem, P_fem_boot) )+
  geom_point()


