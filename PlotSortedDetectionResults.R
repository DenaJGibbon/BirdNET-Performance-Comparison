library(stringr)
library(nasapower)
library(tidyverse)
library(ggridges)
library(scales)  # For time formatting


set.seed(42)

TP <- length(list.files('/Volumes/DJC Files/JahooGibbonBirdNETDetections/CrestedGibbons/Wav/Positive/'))
FP <- length(list.files('/Volumes/DJC Files/JahooGibbonBirdNETDetections/CrestedGibbons/Wav/Negative/'))
TP/(TP+FP) # Precision
TP # number of positive detections

TruePositiveDetections <- list.files('/Volumes/DJC Files/JahooGibbonBirdNETDetections/CrestedGibbons/Wav/Positive/')
TruePositiveDetectionsSplit <-str_split_fixed(TruePositiveDetections, pattern = '_', n=8)

Recorder <- TruePositiveDetectionsSplit[,5]
Date <- TruePositiveDetectionsSplit[,6]
Date <- as.Date(Date, format = "%Y%m%d")
Month <- as.numeric(substr(Date,6,7))
Hour <- substr(TruePositiveDetectionsSplit[,7],1,2)

TruePositiveDF <- cbind.data.frame(Recorder, Date, Month, Hour)
#write.csv(TruePositiveDF,'data/verified_detections.csv',row.names = T)
TruePositiveDF$Hour <- as.numeric((TruePositiveDF$Hour))

ggpubr::gghistogram(data=TruePositiveDF,x='Hour', facet.by="Recorder",stat="count")

ggpubr::gghistogram(data=TruePositiveDF,x='Date', stat="count")

table(TruePositiveDF$Hour,TruePositiveDF$Date)

head(TruePositiveDF)

TruePositiveDF$Second <- round(as.numeric(TruePositiveDetectionsSplit[,3])/100)*100

# Find matching date and hour across different recorders
matching_dates <- TruePositiveDF %>%
  group_by(Date, Hour, Second) %>%
  filter(n_distinct(Recorder) > 1) %>%
  ungroup()

# View the results
print(matching_dates)

# Step 2: Remove duplicates based on Date, Hour, and Second, keeping a random instance
TruePositiveDF <- TruePositiveDF %>%
  group_by(Date, Hour, Second) %>%
  sample_n(1) %>%  # Randomly select 1 row from each group
  ungroup()  # Ungroup to return to the original data frame structure

# View the cleaned data
head(TruePositiveDF)



ByHourRecorderDF <- as.data.frame(table(TruePositiveDF$Recorder,TruePositiveDF$Hour))
colnames(ByHourRecorderDF) <- c('Recorder','Hour', 'Freq')
ByHourRecorderDF$Hour <- as.numeric(as.character(ByHourRecorderDF$Hour ))

# # Adding day/night information
# ByHourRecorderDF <- ByHourRecorderDF %>%
#   mutate(Period = ifelse(Hour >= 6 & Hour <= 18, "Day", "Night"))

# Ridge plot
# Step 1: Create a data frame with all combinations of Recorder and Hour
all_combinations <- expand.grid(Recorder = unique(ByHourRecorderDF$Recorder),
                                Hour = 0:23)

# Step 2: Merge this with the existing data, filling missing combinations with NA
ByHourRecorderDF_full <- merge(all_combinations, ByHourRecorderDF,
                               by = c("Recorder", "Hour"),
                               all.x = TRUE)

# Step 3: Replace NAs in Freq with 0
ByHourRecorderDF_full$Freq[is.na(ByHourRecorderDF_full$Freq)] <- 0

# Ensure Hour is numeric
ByHourRecorderDF_full$Hour <- as.numeric(as.character(ByHourRecorderDF_full$Hour))

# Step 4: Plot the ridge plot
ggplot(ByHourRecorderDF_full, aes(x = Hour, y = Recorder, height = Freq, color = 'grey')) +
  geom_density_ridges(
    stat = "identity", scale = 2, color = "black", alpha = 0.8
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) + # Hour ticks
  labs(
    title = "Hourly Detection Density by Recorder",
    x = "Hour of day",
    y = "Recorder",
    fill = "Recorder"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  xlim(0, 18) +
  guides(fill = 'none')

rain_data <- get_power(
  community = "RE",
  lonlat = c(107.106802, 12.357878),
  dates = c("2022-03-15", "2024-04-09"),
  temporal_api = "DAILY",
  pars = c("PRECTOTCORR","T2M")
)



library(ggplot2)
library(dplyr)
library(patchwork)

# Prepare the histogram plot for True Positives
TruePositivePlot <- ggplot(TruePositiveDF, aes(x = Date)) +
  geom_bar(stat = "count", fill = "lightblue", color = "black", alpha = 0.3) +  # Histogram
  #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  # Adjust x-axis labels
  theme_minimal() +
  labs(x = "Date", y = "Count of True Positives") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(size = 14, face = "bold")
  )+xlab("")

# Prepare the line plot for rainfall
#rain_data$scaled_rainfall <- rain_data$PRECTOTCORR / 10  # Scale the rainfall
rain_data$YYYYMMDD <- as.Date(rain_data$YYYYMMDD, format = "%Y%m%d")

# Define the monsoon period (May to October) for the years in your dataset
monsoon_start_1 <- as.Date("2022-05-01")
monsoon_end_1 <- as.Date("2022-10-31")
monsoon_start_2 <- as.Date("2023-05-01")
monsoon_end_2 <- as.Date("2023-10-31")

# Create the plot
RainPlot <- ggplot(rain_data, aes(x = YYYYMMDD, y = PRECTOTCORR)) +
  geom_line(color = "blue", size = 1) +  # Line graph for scaled rainfall
  scale_y_continuous(name = "Rainfall (mm/day)") +
  labs(x = "Date", y = "Rainfall") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(size = 14, face = "bold")
  ) +
  xlab("") +
  # Add shaded area for the monsoon period across multiple years
  geom_rect(aes(xmin = monsoon_start_1, xmax = monsoon_end_1, ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.01) +  # Light blue shading for 2022
  geom_rect(aes(xmin = monsoon_start_2, xmax = monsoon_end_2, ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.01)    # Light blue shading for 2023

# Display the plot
print(RainPlot)


Tempplot <- ggplot(rain_data, aes(x = YYYYMMDD, y = T2M)) +
  geom_line(color = "red", size = 1) +  # Line graph for scaled rainfall
  scale_y_continuous(
    name = "Temperature (C)"
  ) +
  labs(x = "Date", y = "Temperature") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(size = 14, face = "bold")
  )+xlab("")

# Combine the two plots in a grid (one on top of the other)
combined_plot <- RainPlot / Tempplot/TruePositivePlot

# Print the combined plot
print(combined_plot)

# Step 1: Aggregate the data by Date and Hour to get the total recorders and detections
heatmap_data <- CombinedWeatherData_all %>%
  group_by(Date, Hour) %>%
  summarise(
    total_recorders = n_distinct(Recorder),  # Count distinct recorders
    detections = sum(Count > 0, na.rm = TRUE)  # Count recorders with detections (Count > 0)
  )

# Step 2: Calculate the proportion of recorders with detections
heatmap_data <- heatmap_data %>%
  mutate(proportion_detections = detections / total_recorders)  # Proportion of recorders with detections

# Step 3: Generate all combinations of Date and Hour
complete_dates <- seq.Date(from = min(CombinedWeatherData_all$Date), to = max(CombinedWeatherData_all$Date), by = "day")
complete_hours <- 0:23  # Assuming 24 hours for each day

# Create a dataframe with all combinations of Date and Hour
complete_combinations <- expand.grid(Date = complete_dates, Hour = complete_hours)

# Step 4: Merge with the aggregated data to get missing combinations (and handle them as NA)
combined_data <- merge(complete_combinations, heatmap_data, by = c("Date", "Hour"), all.x = TRUE)

# Step 5: Replace NAs in proportion_detections with NA
combined_data$proportion_detections[is.na(combined_data$proportion_detections)] <- NA

# Ensure that the Hour is numeric
combined_data$Hour <- as.numeric(combined_data$Hour)

# Subset the data to only include hours between 04:00 and 18:00
combined_data <- subset(combined_data, Hour > 4 & Hour < 18)

# Create the heatmap
HeatMap <- ggplot(combined_data, aes(x = Date, y = Hour, fill = proportion_detections)) +
  geom_tile(color = "white") +  # White borders for better visibility
  scale_fill_gradient(low = "white", high = "black", na.value = "red") +  # Greyscale color palette
  scale_y_continuous(
    breaks = seq(5, 17, by = 2),  # Show every other hour
    labels = sprintf("%02d:00", seq(5, 17, by = 2))  # Format labels as HH:00
  ) + labs(x = "Date", y = "Hour (Local Time)", fill = "Proportion of Recorders with Detections") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  ) +
  guides(fill = 'none')

HeatMap

BarPlot <- ggbarplot(data=combined_data,x='Date',y='proportion_detections')+ylab('Proportion of recorders \n with detections')


TruePositiveDFSunrise <- getSunlightTimes(date = TruePositiveDF$Date,
                                          keep = c("sunrise"),
                                          lat = 12.357878, lon = 107.106802, tz = "UTC")


# Ensure 'date' is in Date format
TruePositiveDFSunrise$date <- as.Date(TruePositiveDFSunrise$date)

# Convert 'sunrise' to POSIXct in UTC
TruePositiveDFSunrise$sunrise <- as.POSIXct(TruePositiveDFSunrise$sunrise, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Adjust 'sunrise' for UTC+7 (Bangkok time zone)
TruePositiveDFSunrise$sunrise <- with_tz(TruePositiveDFSunrise$sunrise, tzone = "Asia/Bangkok")

# Extract time part of 'sunrise' and set to a reference date
TruePositiveDFSunrise$time <- as.POSIXct(format(TruePositiveDFSunrise$sunrise, "%H:%M:%S"), format = "%H:%M:%S", tz = "Asia/Bangkok")

# Plot with ggplot2
# https://www.nature.com/articles/nature03259#MOESM1
SunrisePlot <- ggplot(TruePositiveDFSunrise, aes(x = date, y = time)) +
  geom_line() +
  labs(x = "Date", y = "Sunrise Time (UTC+7)", ) +
  theme_minimal()

# Combine the plots: Add the new plot to the left
TimeSunrisePlot <- StandarizedBarplot | (HeatMap / SunrisePlot / RainPlot )

TimeSunrisePlot + plot_annotation(
  tag_levels = "A"  # Automatically labels subplots as A, B, C, etc.
) &
  theme(plot.tag.position  = c(1, 1))


# Monthly detections ------------------------------------------------------

library(dplyr)
library(ggplot2)

# Convert 'Date' to Date type if necessary
CombinedWeatherData_alladdgps$Date <- as.Date(CombinedWeatherData_alladdgps$Date)

# Extract month from Date
CombinedWeatherData_alladdgps$Month <- format(CombinedWeatherData_alladdgps$Date, "%Y-%m")

# Aggregate data by Month, Latitude, Longitude
monthly_detections <- CombinedWeatherData_alladdgps %>%
  group_by(Month, Latitude, Longitude) %>%
  summarise(total_detections = sum(total_count, na.rm = TRUE))  # Sum detections per month per location

ggplot(monthly_detections, aes(x = Longitude, y = Latitude, color = total_detections, size = total_detections)) +
  geom_point() +
  facet_wrap(~ Month, scales = "free", ncol = 4) +  # Facet by month
  scale_color_viridis_c() +  # Choose a color scale
  labs(x = "Longitude", y = "Latitude",
       title = "Detections by Latitude, Longitude, and Month",
       color = "Total Detections", size = "Total Detections") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )


# Example assuming you have a ggplot list `plot_list`
# Load necessary library for quarter extraction
library(lubridate)

# Convert Date to a Date class if it's not already
CombinedWeatherData_alladdgps$Date <- as.Date(CombinedWeatherData_alladdgps$Date)

# Add a 'Quarter' column to the data
CombinedWeatherData_alladdgps$Quarter <- quarter(CombinedWeatherData_alladdgps$Date, with_year = TRUE)

CombinedWeatherData_alladdgps$Year_season <- paste(
                     substr(CombinedWeatherData_alladdgps$Date,1,4),CombinedWeatherData_alladdgps$Season,sep='_')

# Create a new data frame with detections grouped by latitude, longitude, and quarter
monthly_detections <- CombinedWeatherData_alladdgps %>%
  group_by(Year_season, Latitude, Longitude) %>%
  summarise(total_detections = sum(n.detection))

# Set the plotting layout: e.g., 2 rows and 2 columns per page (adjust as needed)

ggplot(monthly_detections, aes(x = Longitude, y = Latitude, color = total_detections, size = total_detections)) +
  geom_point() +
  facet_wrap(~ Year_season, scales = "free", ncol = 2) +  # Facet by month
  scale_color_viridis_c() +  # Choose a color scale
  labs(x = "Longitude", y = "Latitude",
       title = "",
       color = "Total Detections", size = "Total Detections") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )+guides(size='none')
