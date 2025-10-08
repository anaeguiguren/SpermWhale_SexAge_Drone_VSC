library(shiny)
library(dplyr)
library(lubridate)

# Read in flight log data from FLight Reader
d<- read.csv("C:/Users/balae/Documents/dji_mini_measurement_error/Raw_Data/FlightReader_Flight_logs.csv", header = T)

d$GIMBAL.pitch <- as.numeric(d$GIMBAL.pitch)

d$datetime_utc6 <- ymd_hms(d$datetime_utc6, tz = "Etc/GMT+6")

d <- d %>%
  arrange(datetime_utc6, OSD.flyTime..s.)

#keep only the first record of each second
d <- d %>%
  distinct(datetime_utc6, .keep_all = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("Gimbal Pitch < -87° Time Finder"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("video_file", "Enter Video File Name:", "Gal2023_DJIMini2_20230201_091905.MP4"),
      actionButton("submit", "Find Time Ranges")
    ),
    
    mainPanel(
      tableOutput("result_table")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  result <- eventReactive(input$submit, {
    
    # Extract date and time from the video file name
    video_file <- input$video_file
    date_part <- unlist(strsplit(video_file, "_"))[3]  
    time_part <- gsub(".MP4", "", unlist(strsplit(video_file, "_"))[4])
    
    # Compute video start datetime (local time)
    video_start_time <- ymd_hms(paste0(date_part, time_part), tz = "Etc/GMT+6")
    
    # Filter rows where gimbal pitch < -87°
    filtered_data <- d %>%
      filter(GIMBAL.pitch < -87)
    
    if (nrow(filtered_data) == 0) {
      return(data.frame(Message = "No gimbal pitch < -87° found"))
    }
    
    # Compute time in seconds relative to video start
    filtered_data <- filtered_data %>%
      mutate(time_seconds = as.numeric(difftime(datetime_utc6, video_start_time, units = "secs")))
    
    # Identify continuous segments
    filtered_data <- filtered_data %>%
      mutate(group = cumsum(c(1, diff(time_seconds) > 1))) # New group if time gap > 1 sec
    
    # Summarize start and end times for each segment
    result_table <- filtered_data %>%
      group_by(group) %>%
      summarize(start_time_sec = min(time_seconds), end_time_sec = max(time_seconds), .groups = "drop") %>%
      select(start_time_sec, end_time_sec)
    
    # Filter only time intervals within 0 - 720 seconds
    result_table <- result_table %>%
      filter(start_time_sec >= 0, end_time_sec <= 900)
    
    # If no valid intervals, return a message
    if (nrow(result_table) == 0) {
      return(data.frame(Message = "No intervals within 0-900 found"))
    }
    
    
    
    # Convert seconds to mm:ss format
    result_table <- result_table %>%
      mutate(
        start_time = sprintf("%02d:%02d", start_time_sec %/% 60, start_time_sec %% 60),
        end_time = sprintf("%02d:%02d", end_time_sec %/% 60, end_time_sec %% 60)
      ) %>%
      select(start_time, end_time)  # Keep only formatted columns
    
    return(result_table)
  })
  
  
  # Render result table
  output$result_table <- renderTable({
    result()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
