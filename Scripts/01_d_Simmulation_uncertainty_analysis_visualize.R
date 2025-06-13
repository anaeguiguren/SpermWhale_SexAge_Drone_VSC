data <- generate_dataset(uncertainty_sd = 0.02, params.true)


data.ID<- data %>%
  group_by(ID) %>%
  summarise(Length = first(Length), True_ratio = first(true_ratio),
            Ratio = mean(Ratio), Sex = first(Sex), SD_Ratio = first(sd_R))%>%
  mutate(upper = Ratio + SD_Ratio, lower = Ratio - SD_Ratio)



head(data.ID)


# Use optim_sex to fit parameters with weighted regression
fit <- try({
  optim_sex(data.ID, chm = chm, pard0 = pard0, weighted = FALSE)
}, silent = TRUE)


  data.ID$fem_probs <- f_probs(fit$params, data.ID, chm = chm)
  
ggplot(data.ID, aes(x = Length, y = Ratio, colour = Sex))+
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  geom_point(aes(x = Length, y = True_ratio), colour = "black", alpha = 0.5)




ggplot(data.ID, aes(x = Length, y = Ratio, colour = fem_probs))+
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  geom_point(aes(x = Length, y = True_ratio), colour = "black", alpha = 0.5)
