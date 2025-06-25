library(exiftoolr)
library(dplyr)


folder_path <- "E:/Gal2023_Drone/Galapagos2023_Drone_Snapshots/SpermWhale_AgeSex_Snapshots/SpermWhale_Drone_ID_Catalogue/Lightroom_PhotoID_Export"
files <-list.files(folder_path, pattern = ".jpg", full.names = T)
 
exif_data <- exif_read(files, tags = c("FileName","PreservedFileName","ImageDescription", "Keywords","Rating", "Title") )

View(exif_data)


id_dat <- exif_data %>%
  select(FileName, RawFileName = PreservedFileName, Keywords, Caption = ImageDescription, ID = Title, Q =  Rating) %>%
  mutate(across(where(is.list), ~ sapply(., function(x) paste(unlist(x), collapse = ", ")))) %>%
  tibble()



write.csv(id_dat, "Data/Photo_ID_all.csv")
