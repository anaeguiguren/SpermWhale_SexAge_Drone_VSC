# Measure and graph whales:
source("R/functions.R")

#1. read in morphometrix data -----
ROOTfolderpath <- "D:/Gal2023_Drone/Galapagos2023_Drone_Snapshots/SpermWhale_AgeSex_Snapshots"

morpho.output <- getMorphoMetrix(ROOTfolderpath)

#write.csv(morpho.output, "Output_Data/morpho.output.batch2.csv")
#morpho.output <-read.csv("Output_Data/morpho.output.batch2.csv", header = T)

#morpho.output <- read.csv("Output_Data/morpho.output.csv", header = T)

morpho.output<-morpho.output%>% mutate(
  video.file = substr(imageName, 1, 32)
)

morpho.output <- morpho.output %>% mutate(
  video.whale.ID =paste(video.file, ind, sep = "_"),
  date = substr(video.whale.ID, 18,25)
)

# ~~~~a. get correct altitude data from srt fiiles ----
morpho.output <- getSrtAltitude(morpho.output)




# ~~~~b. get length estimates-----
morpho.output<- morpho.output %>% mutate(
  altitude.c = altitudeASL(altitude.raw = droneAltitude), #add launch height to barometer altitude:
  TL.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = TL.px), #estimate length in meters
  HD.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = HD.px), #estimate length in meters
  HF.m = measureWhales(image.width = image_width, altitude = altitudeASL(altitude.raw = droneAltitude),length.pixels = HF.px),
  ratio.HD = HD.px/TL.px, # ratio using nose-dorsal fin measure
  ratio.HF = HF.px/TL.px, # ratio using nose- flipper measure
  ratio.DF = HF.px/HD.px # ratio nose-flipper to nose DF
)

