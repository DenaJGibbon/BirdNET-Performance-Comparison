library(ggpubr)

TempFrame <-  gpx::read_gpx('/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/GPSData/Jahootraining_Bioacoustic Units.gpx')
TempFrame$waypoints$Name <- str_split_fixed(TempFrame$waypoints$Name, pattern = 'UNIT ', n=2)[,2]
GPSPoints <- cbind.data.frame(TempFrame$waypoints$Name,TempFrame$waypoints$Latitude,TempFrame$waypoints$Longitude)
colnames(GPSPoints) <- c('Recorder', 'Latitude', 'Longitude')

CombinedWeatherData_all <- rbind.data.frame(CombinedWeatherData_counts,CombinedWeatherDataNoCall[,c("Date", "Recorder", "Hour", "PRECTOTCORR", "T2M", "Count")])

tail(CombinedWeatherData_all)

CombinedWeatherData_alladdgps <- merge(CombinedWeatherData_all,GPSPoints,by.x = "Recorder", by.y = "Recorder")

head(CombinedWeatherData_alladdgps)

CombinedWeatherData_alladdgps$Month <- substr(CombinedWeatherData_alladdgps$Date,6,7)
range(CombinedWeatherData_alladdgps$Date)
# Calculate hourly rate grouped by Recorder, Month, and Hour, dividing by total recordings
hourly_rate_data <- CombinedWeatherData_alladdgps %>%
  group_by(Recorder, Month, Hour, Latitude, Longitude) %>%
  summarise(
    Average_Count = sum(Count),
    Total_Recordings = n(), # Count the total recordings
    Adjusted_Count = mean(Count) / n(), # Divide mean by total recordings
    .groups = "drop"
  )

tail(hourly_rate_data)

# Plot GPS coordinates with point sizes based on hourly rate
gps_plot <- ggplot(hourly_rate_data, aes(x = Longitude, y = Latitude)) +
  geom_point(aes(size = Adjusted_Count)) +
  labs(
    title = "GPS Coordinates with Points Sized by Hourly Detection Rate",
    x = "Longitude",
    y = "Latitude",
    size = "Hourly Rate"
    #color = "Recorder"
  ) +
  theme_minimal()

# Display the plot
print(gps_plot)

CombinedWeatherData_alladdgps$Count_binary <- ifelse(CombinedWeatherData_alladdgps$Count > 0, 1,0)

# Create the table and convert it to a data frame for ggplot
hourly_rate_data_ignorerecorder <- CombinedWeatherData_alladdgps %>%
  group_by(Date, Month, Hour) %>%
  summarise(
    Sum_Count = sum(Count_binary),
    Total_Recordings = n(), # Count the total recordings
    Adjusted_Count = sum(Count_binary) / n()
  )


# Convert columns to appropriate types
hourly_rate_data_ignorerecorder$Hour <- as.numeric(as.character(hourly_rate_data_ignorerecorder$Hour))
hourly_rate_data_ignorerecorder$Date <- as.Date(as.character(hourly_rate_data_ignorerecorder$Date))

# Convert Hour to a time format
hourly_rate_data_ignorerecorder$Time <- format(strptime(hourly_rate_data_ignorerecorder$Hour, format = "%H"), format = "%H:%M")

# Plot the heatmap
Hourbydateplot <- ggplot(hourly_rate_data_ignorerecorder, aes(x = Date, y = Time, fill = Adjusted_Count)) +
  geom_tile(color = "white") + # White borders for better visibility
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey") + # Gradient for counts
  labs( x = "Date", y = "Local time", fill = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )+
  scale_y_discrete(breaks = c("00:00", "02:00", "04:00", "06:00", "08:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00")) + # Show every other hour
  guides(fill = "none")

Hourbydateplot

TimeSunrisePlot <-  SunrisePlot / RainPlot/Hourbydateplot
print(TimeSunrisePlot)


# Detection probability ---------------------------------------------------

library(dplyr)
library(tidyr)

# Generate a complete sequence of dates and hours
full_dates_hours <- expand.grid(
  Date = seq(min(hourly_rate_data_ignorerecorder$Date),
             max(hourly_rate_data_ignorerecorder$Date),
             by = "day"),
  Hour = 0:23
)

# Ensure `Date` is a Date type in your dataset
hourly_rate_data_ignorerecorder <- hourly_rate_data_ignorerecorder %>%
  mutate(Date = as.Date(Date))

# Join with the full dates and hours and fill missing rows
hourly_rate_data_complete <- full_dates_hours %>%
  left_join(hourly_rate_data_ignorerecorder, by = c("Date", "Hour")) %>%
  mutate(
    Sum_Count = replace_na(Sum_Count, 0),
    Total_Recordings = replace_na(Total_Recordings, 0),
    Adjusted_Count = replace_na(Adjusted_Count, 0),
    Time = ifelse(is.na(Time), Hour, Time) # Fill Time with Hour if missing
  )

# Check the completed dataset
head(hourly_rate_data_complete)


# Prepare the histogram plot for True Positives
ggline(data=hourly_rate_data_complete,x='Date', y='Adjusted_Count')

