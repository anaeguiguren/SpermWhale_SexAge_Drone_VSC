rm(list = ls())

load("bootstrapped_estimates.RData")
source("Scripts/functions.R")
library(wacolors)
library(patchwork)


# 1. processing boot results----
#~~~~a.individual whales----


#~~~~b.curves----

# create lines for each bootstrap:
min.L <- 4 # length at birth
max.L.F <- 16.5 # max length females
max.L.M <- 16.5 #Max male length


#length sequences:
x_F <- seq(min.L, max.L.F, by = 0.2)
x_M <- seq(6, max.L.M, by = 0.2)



mean_curve <- function(df) {
  result <- by(df, df$Length, function(subset) {
    data.frame(
      Length = unique(subset$Length),
      
      #fr
      Ratio = mean(subset$Ratio, na.rm = T)
    )
  })
  
  do.call(rbind, result)
}


# read in males
morpho.males <- read.csv("Data/males_david.csv", header = T)





# get parameters for mean curve

#loop through:
all_f_lines_hf <- list()
all_m_lines_hf <- list()

for(i in 1:nrow(hd_params_hf)){
  
  y_F <- fem_curve(length = x_F,
                   fr = hd_params_hf$fr[i],
                   fmax = hd_params_hf$fmax[i])
  
  
  y_M <- mal_curve(length = x_M,
                   fr = hd_params_hf$fr[i],
                   fmax = hd_params_hf$fmax[i], 
                   mr = hd_params_hf$mr[i], 
                   mmax = hd_params_hf$mmax[i],
                   chm = 6)
  
  f_line_hf <- data.frame(Length = x_F, Bootstrap = i, Sex = "F", Ratio = y_F)
  m_line_hf <- data.frame(Length = x_M, Bootstrap = i, Sex = "M",Ratio = y_M)
  
  #store in big list
  all_f_lines_hf[[i]] <- f_line_hf
  all_m_lines_hf[[i]] <- m_line_hf
  
}


# squish lists into data frames:

f_lines_hf <- do.call(rbind, all_f_lines_hf)
m_lines_hf <- do.call(rbind, all_m_lines_hf)

# make single data frame
all_lines_hf <- rbind(f_lines_hf, m_lines_hf)

summary(all_lines_hf)

f_mean_line_hf <- mean_curve(f_lines_hf)
m_mean_line_hf <- mean_curve(m_lines_hf)


mean_f_line_hf <- data.frame(Length = f_mean_line_hf$Length,
                             Ratio = f_mean_line_hf$Ratio, Sex = "Fem", Bootstrap = 1)

mean_m_line_hf <- data.frame(Length = m_mean_line_hf$Length,
                             Ratio = m_mean_line_hf$Ratio, Sex = "Mal", Bootstrap = 1)

mean_lines_hf <- rbind(mean_f_line_hf, mean_m_line_hf)



#compared to mean params?


mean_hf_params<- hd_params_hf%>%
  dplyr::summarise(across(everything(), median, na.rm =T))


y_F <- fem_curve(length = x_F,
                 fr = mean_hf_params$fr,
                 fmax = mean_hf_params$fmax)


y_M <- mal_curve(length = x_M,
                 fr = mean_hf_params$fr,
                 fmax = mean_hf_params$fmax, 
                 mr = mean_hf_params$mr, 
                 mmax = mean_hf_params$mmax,
                 chm = 6)


s_f_line_hf <- data.frame(Length = x_F, Sex = "F", Ratio = y_F)
s_m_line_hf <- data.frame(Length = x_M, Sex = "M",Ratio = y_M)
s_line_hf_s <- rbind(s_f_line_hf, s_m_line_hf)


# 
ggplot(s_line_hf_s, aes(x = Length, y = Ratio, colour = Sex))+
  geom_line()
  
  
  geom_line(inherit.aes = F, data = mean_line_hf_s, 
            aes(x = Length, y = Ratio, colour = Sex))








p4<-ggplot(all_lines_hf, aes(x = Length, y = Ratio, 
                             group = interaction(Bootstrap, Sex),
                             colour = Sex))+
  #geom_vline(xintercept = c(4, 5.5, 7.5, 8.5, 10, 12, 13.7), alpha = 0.3, linetype = "dashed")+  # Vertical lines
  geom_vline(xintercept = 6, alpha = 0.3)+
  geom_line(alpha = 0.05, linewidth = 0.5)+
  geom_line(data = mean_f_line_hf, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth = 1, alpha = 1) +  # mean lines
  geom_line(data = mean_m_line_hf, aes(x = Length, y = Ratio, colour = Sex), 
            linewidth =1, alpha = 1 ,linetype = "dashed") +  # mean lines
  
  scale_color_manual(values = c("F" = "#123c2e", "M" = "#eba8ad", 
                                "Fem" = "#b8c5c0", "Mal" = "#f9e5e6"))+
  scale_y_continuous(limits = c(0.24, 0.42))+
  #geom_text(data = whaling_lables_hf, aes(x = Length+0.1, label = label),
  #          y = 0.425,
  #          hjust = 0, size = 2.5, inherit.aes = F)+
  theme_classic()+
  labs(x = "Length (m)", y = expression(NR[flipper]), title = "b)")+
  guides(colour = "none")




