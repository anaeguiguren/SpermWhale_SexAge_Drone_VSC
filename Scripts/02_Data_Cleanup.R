# Data Wrangling Script
# This script processes the raw morphometric measurements

# Load required packages
source("Scripts/functions.R")
library(tidyverse)

# 1. Read and clean data -----
#raw reads from Morphometrix
morpho.output <-read.csv("Data/morpho.output.batch2.csv", header = T)


# get .mp4 file name
morpho.output<-morpho.output%>% mutate(
  video.file = substr(imageName, 1, 32)
)

# get within-video whale ID and date
morpho.output <- morpho.output %>% mutate(
  video.whale.ID =paste(video.file, ind, sep = "_"),
  date = substr(video.whale.ID, 18,25)
)

# get correct altitude data from drone srt files 
morpho.output <- getSrtAltitude(morpho.output)

# get and ration length estimates
morpho.output<- morpho.output %>% mutate(
  altitude.c = altitudeASL(altitude.raw = droneAltitude), #add launch height to barometer altitude:
  TL.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = TL.px), #estimate length in meters
  HD.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = HD.px), #estimate length in meters
  HF.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = HF.px),
  ratio.HD = HD.px/TL.px, # ratio using nose-dorsal fin measure
  ratio.HF = HF.px/TL.px, # ratio using nose- flipper measure
  ratio.DF = HF.px/HD.px # ratio nose-flipper to nose DF
)

# add quality ratings: 
# get q. ratings
q.ratings <- read.csv("Data/Processed_Data/morpho.output.QRating.csv", header = T)
q.ratings <- q.ratings %>% mutate(imageName = file_name)


morpho.output <- morpho.output %>%
  mutate(ind = as.character(ind)) %>%
  left_join(q.ratings %>% mutate(ind = as.character(ind)) %>%
              select(imageName, ind, Q.focus, Q.straightness, Q.roll, Q.arch, Q.pitch, Q.meas, Q.dorsal, Q.flippers ), 
            by = c("imageName", "ind") )

# remove Balaena measurement

morpho.output<- morpho.output[-which(is.na(morpho.output$Q.focus & morpho.output$notes == "balaena")),]


morpho.output <- morpho.output %>% filter(notes != "balaena")

# get photo-id information: 

photo.id <- read.csv("Data/Photo_ID_all.csv", header = T)

photo.id <- photo.id %>%
  mutate(ind = str_extract(Keywords, "ind\\d+") %>% str_remove("ind"), 
         video.file = substr(RawFileName, 1, 32),
         video.whale.ID = paste(video.file, ind, sep = "_"),
         snapshot.ID= paste(RawFileName, ind, sep = "_"))


photo.id <-photo.id %>%
  mutate(class = case_when(
    str_detect(Keywords, "male") ~ "male",
    str_detect(Keywords, "juanito") ~ "juanito",
    TRUE ~ NA
  ))


# error check: 
photo.id %>%
  group_by(ID) %>%
  summarize(unique_inds = n_distinct(Caption), .groups = "drop") %>%
  filter(unique_inds > 1)  # nice!


# merge to morpho out

morpho.output <-morpho.output %>%
  mutate(snapshot.ID = paste(imageName, ind, sep = "_"))

morpho.output<- left_join(morpho.output, photo.id, by  ="snapshot.ID")

# remove rogue duplicate
morpho.output <- morpho.output %>%
  distinct(snapshot.ID, .keep_all = TRUE)

#remove image with no quality rating
morpho.output <-morpho.output %>%
  filter(!is.na(Q))

# identifiable whales only:

id.morph <- morpho.output%>%
  filter(!is.na(ID))

id.mean <- id.morph %>%
  group_by(ID) %>%
  summarize(mean_TL = mean(TL.m, na.rm = T), cv_TL = (sd(TL.m, na.rm = T)/mean_TL)*100, sd_TL = sd(TL.m, na.rm = T),
            mean_HD = mean(HD.m, na.rm = T), cv_HD = (sd(HD.m, na.rm = T)/mean_HD)*100, sd_HD = sd(HD.m, na.rm = T),
            mean_HF = mean(HF.m, na.rm = T), cv_HF = (sd(HF.m, na.rm = T)/mean_HF)*100, sd_HF = sd(HD.m, na.rm = T),
            mean_ratio.HD = mean(ratio.HD, na.rm = T), cv_ratio.HD = (sd(ratio.HD, na.rm = T)/mean_ratio.HD), sd_ratio.HD = sd(ratio.HD, na.rm = T),
            mean_ratio.HF = mean(ratio.HF, na.rm = T), cv_ratio.HF = (sd(ratio.HF, na.rm = T)/mean_ratio.HF), sd_ratio.HF = sd(ratio.HF, na.rm = T),
            n_photos = n(),
            date = first(date),
            mean_altitude= mean(altitude.c))


hd<-id.mean%>%
  filter(n_photos>1 & !is.na(mean_HD))

hf<-id.mean%>%
  filter(n_photos>1 & !is.na(mean_HF))


#save
write.csv(id.mean, "Data/Processed_Data/id_morpho_output_clean_processed.csv")

# 2. Explore Quality and identifyiablity-----
# Load required libraries
library(ggplot2)
library(ggpubr)
library(dplyr)


# Create boxplot with significance indicators
# this makes sense to do median for each snapshot


p1<- ggplot(morpho.output, aes(x = as.factor(Q), y = altitude.c, fill = as.factor(Q))) +
  geom_boxplot() +
  geom_jitter(width = 0.12, alpha = 0.6) +
  scale_fill_brewer(palette = "Greens") +
  theme_classic()+
  theme(legend.position = "none") +
  geom_hline(yintercept = 70, lty = "dashed")+
  labs(x = "Q rating", y = "Altitude (m)")
  # Adjust label positions

p1

ggsave("Figures/Altitude_vs_Quality.png",
       p1, width = 5, height = 5)


