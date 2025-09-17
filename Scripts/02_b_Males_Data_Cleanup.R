#script processes Males droned by DG in the Arctic and the Gully
# 1. Get data from Morphometrix -----
getMorphoMetrix.mal  <- function(ROOTfolderpath){
  require(dbplyr); require(dplyr)
  #' @title compMorphometrix
  #' @description compile the csv outputs from Morphometrix folder 
  #' @param ROOTfolderpath 	a character vector of full path names to the folder where the csv outputs are located  
  
  
  #remove / from ROOTFfolderpath
  if (substring(ROOTfolderpath, nchar(ROOTfolderpath), nchar(ROOTfolderpath)) == 
      "/") {
    ROOTfolderpath = substring(ROOTfolderpath, 1, nchar(ROOTfolderpath) - 
                                 1)
  }
  
  #folder not found
  if(isFALSE(dir.exists(ROOTfolderpath)))
    stop(paste(ROOTfolderpath, "doesn't exist", sep = ""))
  
  #get list of files
  tmp.files <- list.files(path = ROOTfolderpath, pattern = "*.csv", 
                          full.names = TRUE, recursive = TRUE)
  
  #make empty data table to populate
  #empty flightlog
  morpho.output <- data.frame(imagePath = character(), 
                              videoFile = character(),
                              timeStamp = numeric(),
                              altitude = numeric(),
                              pixelDimension = numeric(),
                              focalLength = numeric(),
                              object = character(),
                              length = numeric(),
                              notes = character()
  )
  
  # go through all files
  for (i in seq_along(tmp.files)){
    
    #prints progress bar
    cat(paste("\r", round(100 * (i/length(tmp.files)), 1), 
              "% processing, file", i, ":", (tmp.files[i])))
    
    #read in each file
    
    a <- (read.csv(tmp.files[i], header = T))
    names(a)
    
    tmp.table <- data.frame(imagePath = unname(a %>% filter(Object == "Image Path") %>% select(Value)), 
                            altitude = as.numeric(unname(a %>% filter(Object == "Altitude") %>% select(Value))), 
                            pixelDimension = as.numeric(unname(a %>% filter(Object == "Pixel Dimension") %>% select(Value))), 
                            focalLength = as.numeric(unname(a %>% filter(Object == "Focal Length") %>% select(Value))), 
                            TL = as.numeric(unname(a %>% filter(Object == "TL" & Value_unit == "Meters")%>%select(Value))), 
                            TL.px = as.numeric(unname(a %>% filter(Object == "TL" & Value_unit == "Pixels")%>%select(Value))), 
                            HF = as.numeric(unname(a %>% filter(Object == "HF" & Value_unit == "Meters")%>%select(Value))),
                            HF.px = as.numeric(unname(a %>% filter(Object == "HF" & Value_unit == "Pixels")%>%select(Value)))
                            
                          )
    
    tmp.table <- tmp.table %>% 
      mutate(R.HF = HF.px/TL.px)

    morpho.output <- rbind(morpho.output, tmp.table)
    
  }
  return(morpho.output)
  
}


morpho.output.males<-getMorphoMetrix.mal(ROOTfolderpath = "D:/Gal2023_Drone/Galapagos2023_Drone_Snapshots/SpermWhale_AgeSex_Snapshots/Male_Sperms_Gully_Arctic")


#remove tag data

morpho.output.males<-morpho.output.males[complete.cases(morpho.output.males),]

# get file basename
morpho.output.males <- morpho.output.males %>%
  mutate(VideoFile = paste(gsub('\\..*', "",basename(morpho.output.males$imagePath), ".MOV"), sep = ""))


# get average for male with 2 measurements

male.mean <- morpho.output.males %>%
  group_by(VideoFile) %>%
  summarize(TL.m = mean(TL), 
            R.HF.m = mean(R.HF), 
            n_photos = n())
rm(getMorphoMetrix.mal)
rm(morpho.output.males)
write.csv(male.mean, "Data/males_david.csv")

                   