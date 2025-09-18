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

# get and ratio and length estimates
morpho.output<- morpho.output %>% mutate(
  altitude.c = altitudeASL(altitude.raw = droneAltitude), #add launch height to barometer altitude:
  TL.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = TL.px), #estimate length in meters
  HD.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = HD.px), #estimate length in meters
  HF.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = HF.px),
  ratio.HD = HD.px/TL.px, # ratio using nose-dorsal fin measure
  ratio.HF = HF.px/TL.px, # ratio using nose- flipper measure
  ratio.DF = HF.px/HD.px # ratio nose-flipper to nose DF
)

# get quality ratings
q.ratings <- read.csv("Data/Processed_Data/morpho.output.QRating.csv", header = T)
q.ratings <- q.ratings %>% mutate(imageName = file_name)


morpho.output <- morpho.output %>%
  mutate(ind = as.character(ind)) %>%
  left_join(q.ratings %>% mutate(ind = as.character(ind)) %>%
              select(imageName, ind, Q.focus, Q.straightness, Q.roll, Q.arch, Q.pitch, Q.meas, Q.dorsal, Q.flippers ), 
            by = c("imageName", "ind") )

# remove Balaena measurement

morpho.output<- morpho.output[-which(is.na(morpho.output$Q.focus & morpho.output$notes == "balaena")),]


# get suckle dives

suckles <- read.csv("Data/morpho.output.nursing.csv", header =T)

suckles <- suckles %>%
  filter(!is.na(X))

morpho.output$suckled <- suckles$nursed
morpho.output$suckling <- suckles$nursing



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
  summarize(unique_inds = n_distinct(ind), .groups = "drop") %>%
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

# print number of stills measured:

cat("Number of stills images:", nrow(morpho.output), "\n")

# number of instances when hf is available:
length(which(!is.na(morpho.output$ratio.HF)))


cat("number of stills with hd:", nrow(morpho.output %>% filter(!is.na(ratio.HD))), "\n")
cat("number of stills with hf:", nrow(morpho.output %>% filter(!is.na(ratio.HF))), "\n")

# number of instances when sd is available:
length(which(!is.na(morpho.output$ratio.HD)))



# identifiable whales only:

id.morph <- morpho.output %>%
  filter(!is.na(ID))

cat("Number of stills images with IDs:", nrow(id.morph), "\n")
cat("Number Identified whals:", length(levels(as.factor(id.morph$ID))), "\n")


#convert suckling to true or false: 
id.morph <- id.morph %>%
  mutate(suckled = ifelse(suckled !="yes", F, T),
         suckling = ifelse(suckling !="yes", F, T))

summary(id.morph$suckled)

id.morph$suckled[which(is.na(id.morph$suckled))] <-F
id.morph$suckling[which(is.na(id.morph$suckling))] <-F

summary(id.morph$suckled)
summary(id.morph$suckling)

#create 'ever suckling' column


id.morph<- id.morph %>%
  group_by(ID) %>%
  mutate(suckled_ever = any(suckled, na.rm = TRUE),
         suckling_ever = any(suckling, na.rm = TRUE)) %>%
  ungroup()
summary(id.morph$suckled_ever)
summary(id.morph$suckling_ever)




write.csv(id.morph, "Data/Processed_Data/id_unpooled_clean_processed.csv")

id.mean <- id.morph %>%
  group_by(ID) %>%
  summarize(mean_TL = mean(TL.m, na.rm = T), cv_TL = (sd(TL.m, na.rm = T)/mean_TL), sd_TL = sd(TL.m, na.rm = T),
            mean_HD = mean(HD.m, na.rm = T), cv_HD = (sd(HD.m, na.rm = T)/mean_HD), sd_HD = sd(HD.m, na.rm = T),
            mean_HF = mean(HF.m, na.rm = T), cv_HF = (sd(HF.m, na.rm = T)/mean_HF), sd_HF = sd(HD.m, na.rm = T),
            mean_ratio.HD = mean(ratio.HD, na.rm = T), cv_ratio.HD = (sd(ratio.HD, na.rm = T)/mean_ratio.HD), sd_ratio.HD = sd(ratio.HD, na.rm = T),
            mean_ratio.HF = mean(ratio.HF, na.rm = T), cv_ratio.HF = (sd(ratio.HF, na.rm = T)/mean_ratio.HF), sd_ratio.HF = sd(ratio.HF, na.rm = T),
            n_photos = n(),
            date = first(date),
            mean_altitude= mean(altitude.c), 
            suckled_ever = first(suckled_ever), 
            suckling_ever = first(suckling_ever))


summary(id.mean$suckled_ever)


hd<-id.mean%>%
  filter(n_photos>2 & !is.na(mean_HD))


hf<-id.mean%>%
  filter(n_photos>2 & !is.na(cv_ratio.HF))


hf %>%
  summarize(
    n_photo_mean = mean(n_photos),
    cv_length_mean = mean(cv_TL), 
    sd_length_mean = sd(cv_TL), 
    cv_ratio.HF_mean = mean(cv_ratio.HF),
    cv_ratio.HD_mean = mean(cv_ratio.HD),
    sd_ratio.HF_mean = mean(sd_ratio.HF)
  )

#save
write.csv(id.mean, "Data/Processed_Data/id_morpho_output_clean_processed.csv")

# 2. Explore Quality and identifyiablity-----
# Load required libraries
library(ggplot2)
library(wacolors)


# Create boxplot with significance indicators
# this makes sense to do median for each snapshot
library(RColorBrewer)
morpho.output<-morpho.output%>%
  mutate(idable = ifelse(as.numeric(Q)<3, "no", "yes"))


p1<- ggplot(morpho.output, aes(x = as.factor(Q), y = altitude.c, fill = idable)) +
  geom_boxplot() +
  #scale_fill_wa_d("coast")+
  scale_fill_grey(start = 0.8, end = 0.3)+
  geom_hline(yintercept = 70, lty = "dashed")+
  labs(x = "Q rating", y = "Corrected altitude (m)", fill = "ID possible")+
  theme_classic() +
  theme(legend.position = "bottom")

  # Adjust label positions

p1

ggsave("Figures/Altitude_vs_Quality.png",
       p1, width = 3.5, height = 3.5)


