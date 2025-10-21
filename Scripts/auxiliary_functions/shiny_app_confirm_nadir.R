library(shiny)
library(dplyr)
library(lubridate)


# Read in flight log data from FLight Reader
#replace path with your path to the flightreader logs
d<- read.csv("C:/Users/balae/Documents/dji_mini_measurement_error/Raw_Data/FlightReader_Flight_logs.csv", header = T)

d$GIMBAL.pitch <- as.numeric(d$GIMBAL.pitch)

d$datetime_utc6 <- ymd_hms(d$datetime_utc6, tz = "Etc/GMT+6")

d <- d %>%
  arrange(datetime_utc6, OSD.flyTime..s.)

#keep only the first record of each second
d <- d %>%
  distinct(datetime_utc6, .keep_all = TRUE)


# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Gimbal Angle Finder"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("video_file", "Enter Video File Name:", "Gal2023_DJIMini2_20230201_091905.MP4"),
      textInput("snapshot_time", "Enter Snapshot Time (mm:ss):", value = "03:40"),
      actionButton("submit", "Find Gimbal Angle")
    ),
    
    mainPanel(
      tableOutput("result_table")
    )
  )
)

# Define the Shiny Server
server <- function(input, output) {
  
  result <- eventReactive(input$submit, {
    
    # Extract date and time from the video file name
    video_file <- input$video_file
    
    # Convert mm:ss to seconds
    parse_mmss <- function(time_str) {
      parts <- unlist(strsplit(time_str, ":"))
      if (length(parts) != 2) return(NA)  # Return NA if format is incorrect
      minutes <- as.numeric(parts[1])
      seconds <- as.numeric(parts[2])
      if (is.na(minutes) || is.na(seconds)) return(NA)  # Handle non-numeric cases
      return(minutes * 60 + seconds)
    }
    
    snapshot_time <- parse_mmss(input$snapshot_time)
    if (is.na(snapshot_time)) {
      return(data.frame(Message = "Invalid mm:ss format. Use MM:SS (e.g., 03:40)"))
    }
    
    date_part <- unlist(strsplit(video_file, "_"))[3]  
    time_part <- gsub(".MP4", "", unlist(strsplit(video_file, "_"))[4])
    
    # Compute snapshot local datetime
    snapshot_datetime <- ymd_hms(paste0(date_part, time_part), tz = "Etc/GMT+6") + round(snapshot_time)
    
    # Filter the data for the closest timestamp match
    out <- d %>%
      filter(datetime_utc6 == snapshot_datetime) %>%
      select(datetime_utc6, GIMBAL.pitch, OSD.height..m.)
  
    
    # Return the first matching row (if exists)
    if (nrow(out) > 0) {
      out$datetime_utc6 <- format(out$datetime_utc6, "%Y-%m-%d %H:%M:%S")
      return(out[1, ])
    } else {
      return(data.frame(Message = "No matching data found"))
    }
  })
  
  # Render the output table
  output$result_table <- renderTable({
    result()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
