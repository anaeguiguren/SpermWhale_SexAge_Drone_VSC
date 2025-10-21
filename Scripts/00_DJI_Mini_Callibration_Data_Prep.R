#Gal23_DroneMeasurementError data prep:
# Morphometric analyses:
source("Scripts/functions.R")


# 1. import and clean data ----
# morphometrix measurements:
m <- read.csv("Data/Callibration_Data/Balaena_Raw_Measures.csv", header = TRUE)

#snapshot metadata
s  <- read.csv("Data/Callibration_Data/Snapshot_Metadata_2023.csv", header = T)
s$imageName <- s$FileName


a <- left_join(m, s, by ="imageName")

#make a column that contains date and video name
a$videoDate <-paste(a$videoFile, a$Date, sep = "_")

# merge data with flight number:
f <- read.csv("Data/Callibration_Data/Drone_Flights_Summary_2023.csv", header = T)

f$videoFileBase <- basename(f$videoFile)

f$Date <- gsub(" .*", "", f$LocalTime)

f$Month <- as.character(month(ymd(010101) + months(as.numeric(substr(f$Date, 7,7))-1),label=TRUE,abbr=TRUE))
f$DateWord <- paste(substr(f$Date, 9,10), f$Month, "23", sep = "-")

f$videoDate <- paste(f$videoFileBase, f$DateWord, sep = "_")

#join:
dat <- left_join(a, f, by = "videoDate")

#cleanup

dat$notes <-tolower(dat$notes)
dat$notes <-trimws(dat$notes)
dat <- dat[-which(dat$notes==""),]
dat$pos <- factor(dat$notes)
dat$balaenaAngled <- factor(dat$balaenaAngled)
dat$FlightNo <- factor(dat$FlightNo)

# 2. clean up
dat <- dat %>% select(
  imageName, timeStamp, LocalTime ,altitude, length ,pixelDimension.old = pixelDimension, ImageWidth ,
  balaenaAngled, pos, FlightNo, Date = Date.y
)

# 3. assign gimbal angle -----
#~~~~a. read in flight logs----
#Define the folder path where flight logs are stored
#FLIGHTREADfolderpath <- "D:/Gal2023_Drone/FlightReader_logs/"

# Read all CSV files and merge into a single dataframe
#tmp.files <- list.files(path = FLIGHTREADfolderpath, pattern = "*.csv", full.names = TRUE, recursive = T)
#df_list <- lapply(tmp.files, read.csv)
#df_list <- lapply(df_list, function(df) {
#  df %>% mutate(across(everything(), as.character))
#})



#d <- bind_rows(df_list)

# Convert datetime to proper format
#d$datetime_utc6 <- mdy_hms(d$CUSTOM.updateDateTime24, tz = "Etc/GMT+6")

# Arrange data by timestamp
#d <- d %>%
#  arrange(datetime_utc6, OSD.flyTime..s.) %>%
#  select(OSD.flyTime..s., datetime_utc6, OSD.height..m., GIMBAL.pitch)

#write.csv(d, "Raw_Data/FlightReader_Flight_logs.csv")

d<- read.csv("Data/Callibration_Data/FlightReader_Flight_logs.csv", header = T)

d <- d %>%
  distinct(datetime_utc6, .keep_all = TRUE)

d$GIMBAL.pitch <- as.numeric(d$GIMBAL.pitch)

d$datetime_utc6 <- ymd_hms(d$datetime_utc6, tz = "Etc/GMT+6")


#~~~~b.format snapshot date times----
dat$videoLocalTime <- ymd_hms(dat$LocalTime, tz ="Etc/GMT+6") #

# add snapshot time stamp:


dat <- dat %>% mutate(
  timeParts = strsplit(timeStamp, ":"), 
  minutes = as.numeric(sapply(timeParts, `[`, 1)),
  seconds = as.numeric(sapply(timeParts, `[`, 2)),
  # Convert to total seconds and add to datetime
  datetime_utc6 = videoLocalTime + minutes*60 +seconds
)



#~~~~c. join based on datetime ------
dat <- left_join(dat, d, by = "datetime_utc6")

#check altitude errors

dat$altitude_error <- dat$altitude-as.numeric(dat$OSD.height..m.)

#hist(dat$altitude_error, breaks = 20)

#x<-dat[sample(which(abs(dat$altitude_error)>10), 10, replace = F),]

#x<-dat[which(abs(dat$altitude_error)>10),] # omit the weird ones 
#x <-dat[which(is.na(dat$GIMBAL.pitch)),]

# classify photos as nadir, true or false:


dat$nadir <- ifelse(dat$GIMBAL.pitch < -87, T, F)

head(dat)

rm(a);rm(f);rm(m);rm(s)

write.csv(dat, ""C:\Users\balae\Documents\SpermWhale_SexAge_Drone_VSC\Data\Callibration_Data\Snapshot_Metadata_2023.csv"")
dat
