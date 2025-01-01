library(stringr)
library(nasapower)

TP <- length(list.files('/Volumes/DJC Files/JahooGibbonBirdNETDetections/CrestedGibbons/Wav/Positive/'))
FP <- length(list.files('/Volumes/DJC Files/JahooGibbonBirdNETDetections/CrestedGibbons/Wav/Negative/'))
TP/(TP+FP) # Precision

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

ggpubr::gghistogram(data=TruePositiveDF,x='Date', stat="count",facet.by="Recorder")

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

# Here you would proceed with your modeling code using 'unique_detections'

print(unique_detections)



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
  xlim(0, 23) +
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

RainPlot<- ggplot(rain_data, aes(x = YYYYMMDD, y = PRECTOTCORR)) +
  geom_line(color = "blue", size = 1) +  # Line graph for scaled rainfall
  scale_y_continuous(
    name = "Rainfall (mm/day)"
  ) +
  labs(x = "Date", y = "Rainfall") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(size = 14, face = "bold")
  )+xlab("")

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

# Extract and print the time
library(lubridate)
library(ggplot2)

library(suncalc)
library(ggplot2)
library(lubridate)

library(ggplot2)
library(reshape2)

# Create the table and convert it to a data frame for ggplot
heatmap_data <- as.data.frame(as.table(table(TruePositiveDF$Hour, TruePositiveDF$Date)))
# Rename the columns for better understanding
colnames(heatmap_data) <- c("Hour", "Date", "Count")

heatmap_data$Count <- ifelse(heatmap_data$Count==0,0,1)

# Convert columns to appropriate types
heatmap_data$Hour <- as.numeric(as.character(heatmap_data$Hour))
heatmap_data$Date <- as.Date(as.character(heatmap_data$Date))

# Plot the heatmap
Hourbydateplot <- ggplot(heatmap_data, aes(x = Date, y = Hour, fill = Count)) +
  geom_tile(color = "white") + # White borders for better visibility
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey") + # Gradient for counts
  labs( x = "Date", y = "Hour", fill = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )+guides(fill="none")

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

TimeSunrisePlot <-  SunrisePlot / RainPlot/Hourbydateplot
print(TimeSunrisePlot)
