# Summarize peduncle dive analyses

library(dplyr)


#1. read in data ----
#checks weather individuals were seen peduncle diving 
#in a given video

suckles <- read.csv("Data/morpho.output.nursing.csv", header =T)


#2. get names of vide files ----
suckles<- suckles %>%
  mutate(video.File = paste(substr(video.whale.ID, 1, 32), ".mp4", sep = ""))


#3. check which videos were used -----

pd.videos<- suckles %>%
  group_by(video.File)%>%
  summarize(pd.checked = first(nursed))

pd.videos <- pd.videos[-1,]

# manually check NA videos - they probably just have one whale
na.videos <- 
  pd.videos %>%
  filter(is.na(pd.checked))

#4. make a copy of videos inspected ------
#make a full name
pd.videos$full.video.File<- paste(
  "E:/Gal2023_Drone/Drone_2023/",
  substr(pd.videos$video.File, 22, 25), 
  "2023/", 
  pd.videos$video.File,
  sep = ""
)


file.copy(from = pd.videos$full.video.File, 
          to = "E:/Gal2023_Drone/Measured_Videos_for_pd_analysis/")

