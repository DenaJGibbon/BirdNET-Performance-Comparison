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

CombinedWeatherData_alladdgps$Date_hour <- paste(CombinedWeatherData_alladdgps$Date,CombinedWeatherData_alladdgps$Hour,sep='_')


# Create the table and convert it to a data frame for ggplot
hourly_rate_data_ignorerecorder <- CombinedWeatherData_alladdgps %>%
  group_by(Date_hour) %>%
  summarise(
    Sum_Count = sum(Count_binary),
    Total_Recordings = n(), # Count the total recordings
    Adjusted_Count = sum(Count_binary) / n()
  )

hourly_rate_data_ignorerecorder$Hour <-str_split_fixed(hourly_rate_data_ignorerecorder$Date_hour,pattern = '_', n=2)[,2]
hourly_rate_data_ignorerecorder$Date <-str_split_fixed(hourly_rate_data_ignorerecorder$Date_hour,pattern = '_', n=2)[,1]

# Convert columns to appropriate types
hourly_rate_data_ignorerecorder$Hour <- as.numeric(as.character(hourly_rate_data_ignorerecorder$Hour))
hourly_rate_data_ignorerecorder$Date <- as.Date(as.character(hourly_rate_data_ignorerecorder$Date))

# Convert Hour to a time format
hourly_rate_data_ignorerecorder$Time <- format(strptime(hourly_rate_data_ignorerecorder$Hour, format = "%H"), format = "%H:%M")

# Plot the heatmap
Hourbydateplot <- ggplot(hourly_rate_data_ignorerecorder, aes(x = Date, y = Time, fill = Adjusted_Count)) +
  geom_tile(color = "white") + # White borders for better visibility
  scale_fill_gradient(low = "white", high = "black", na.value = "blue") + # Gradient for counts
  labs( x = "Date", y = "Local time", fill = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )+
  scale_y_discrete(breaks = c("04:00", "06:00", "08:00", "10:00", "12:00", "14:00", "16:00")) + # Show every other hour
  guides(fill = "none")

Hourbydateplot

TimeSunrisePlot <-  SunrisePlot /Hourbydateplot
print(TimeSunrisePlot)


