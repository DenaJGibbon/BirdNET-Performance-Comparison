# Load required libraries
library(stringr)
library(nasapower)
library(tidyverse)
library(ggridges)
library(mgcv)
library(ggpubr)
library(dplyr)
library(gpx)

# Data preparation --------------------------------------------------------

# Define paths
positive_path <- '/Volumes/DJC Files/JahooGibbonBirdNETDetections/CrestedGibbons/Wav/Positive/'
negative_path <- '/Volumes/DJC Files/JahooGibbonBirdNETDetections/CrestedGibbons/Wav/Negative/'

# Calculate Precision and number of true positive detections
TP <- length(list.files(positive_path))
FP <- length(list.files(negative_path))
TP / (TP + FP)  # Precision
TP  # Number of true positive detections

# Process true positive detections
TruePositiveDetections <- list.files(positive_path)
TruePositiveDetectionsSplit <- str_split_fixed(TruePositiveDetections, pattern = '_', n=8)

# Extract components (Recorder, Date, Hour)
Recorder <- TruePositiveDetectionsSplit[,5]
Date <- as.Date(TruePositiveDetectionsSplit[,6], format = "%Y%m%d")
Month <- as.numeric(substr(Date, 6, 7))
Hour <- as.numeric(substr(TruePositiveDetectionsSplit[,7], 1, 2))

# Create a data frame with the extracted components
TruePositiveDF <- data.frame(Recorder, Date, Month, Hour)
nrow(TruePositiveDF)


# Round seconds and add as a new column
TruePositiveDF$Second <- round(as.numeric(TruePositiveDetectionsSplit[,3]) / 300) * 300

nrow(TruePositiveDF)

# # Remove duplicates (based on Date, Hour, Second) and keep a random instance
TruePositiveDF <- TruePositiveDF %>%
  group_by(Date, Hour, Second) %>%
  sample_n(1) %>%  # Randomly select one row from each group
  ungroup()

nrow(TruePositiveDF)

# Fetch weather data from NASA
rain_data <- get_power(
  community = "RE",
  lonlat = c(107.106802, 12.357878),
  dates = c("2022-03-13", "2024-04-09"),
  temporal_api = "DAILY",
  pars = c("PRECTOTCORR", "T2M")
)

# Process detection files without calls
JahooFilesBase <- list.files("/Users/denaclink/Desktop/RStudioProjects/BirdNET-Performance-Comparison/data/JahooBirdNETupdatedmodel", pattern = '.txt', recursive = T, full.names = F)
JahooFilesBase <- str_split_fixed(basename(JahooFilesBase), pattern = '.BirdNET', n = 2)[,1]

TruePositiveDetectionsShort <- str_split_fixed(list.files(positive_path), pattern = '_R', n = 2)[,2]
TruePositiveDetectionsShort <- str_split_fixed(TruePositiveDetectionsShort, pattern = '_.wav', n = 2)[,1]
TruePositiveDetectionsName <- paste('R', TruePositiveDetectionsShort, sep = '')

JahooFilesBaseNocall <- JahooFilesBase[-which(JahooFilesBase %in% TruePositiveDetectionsName)]
JahooFilesBaseNocallSplit <- str_split_fixed(JahooFilesBaseNocall, pattern = '_', n = 4)

# Create a data frame for non-call detections
Recorder <- JahooFilesBaseNocallSplit[,1]
Date <- as.Date(JahooFilesBaseNocallSplit[,2], format = "%Y%m%d")
Month <- as.numeric(substr(Date, 6, 7))
Hour <- substr(JahooFilesBaseNocallSplit[,3], 1, 2)
Second <- substr(JahooFilesBaseNocallSplit[,3], 5, 6)

JahooNoCallDF <- data.frame(Recorder, Date, Month, Hour, Second)

table(JahooNoCallDF$Hour)

TruePositiveDF$Hour <- as.numeric(TruePositiveDF$Hour)
table(TruePositiveDF$Hour )

TruePositiveDF$Time <- format(strptime(TruePositiveDF$Hour, format = "%H"), format = "%H:%M")
TruePositiveDF$Time <- factor(TruePositiveDF$Time, levels = sort(unique(TruePositiveDF$Time)))

# Visualize hourly detection distribution
ggpubr::gghistogram(data = TruePositiveDF, x = 'Time', stat = "count")+
  ylab('True positive detections')

ggpubr::gghistogram(data = TruePositiveDF, x = 'Date', stat = "count")

# Create a table for total number of hours sampled (denominator)
total_hours <- table(JahooNoCallDF$Hour)

# Create a table for true positives (numerator)
true_positives <- table(TruePositiveDF$Hour)

# Align the data by hour
hours <- as.numeric(names(total_hours))  # Extract hour labels
true_positives <- true_positives[as.character(hours)]  # Match hours in the two tables
true_positives[is.na(true_positives)] <- 0  # Handle hours with no detections

# Calculate standardized rate (True Positives per Total Hours)
standardized_rate <- true_positives / total_hours

# Create a data frame for visualization
rate_df <- data.frame(
  Hour = as.numeric(names(total_hours)),
  TruePositives = as.numeric(true_positives),
  TotalHours = as.numeric(total_hours),
  StandardizedRate = as.numeric(standardized_rate)
)

rate_df$Time <- format(strptime(rate_df$Hour, format = "%H"), format = "%H:%M")
rate_df$Time <- factor(rate_df$Time, levels = sort(unique(rate_df$Time)))

# Filter the data frame for hours between 04:00 and 18:00
rate_df <- rate_df[rate_df$Hour >= 4 & rate_df$Hour <= 18, ]

# Create the bar plot
StandarizedBarplot <- ggbarplot(data = rate_df, x = 'Time', y = 'StandardizedRate') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Gibbon detections \n per hour') +
  xlab('Local time')

# Display the plot
print(StandarizedBarplot)

StandarizedBarplot <- ggbarplot(data=rate_df,x='Time',y='StandardizedRate')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +ylab('Gibbon detections \n per hour')+xlab('Local time')

# Combine with weather data for non-call detections
CombinedWeatherDataNoCall <- merge(JahooNoCallDF, rain_data, by.x = "Date", by.y = "YYYYMMDD")
CombinedWeatherDataNoCall$Count <- 0

# Combine true positive detections with weather data
CombinedWeatherData <- merge(TruePositiveDF, rain_data, by.x = "Date", by.y = "YYYYMMDD")

nrow(CombinedWeatherData)

# Count detections by Recorder, Date, and Hour
CombinedWeatherData_counts <- CombinedWeatherData %>%
  group_by(Date, Recorder, Hour, PRECTOTCORR, T2M) %>%
  summarise(Count = n(), .groups = "drop")

# Combine positive and non-call data
CombinedWeatherData_all <- rbind(CombinedWeatherData_counts, CombinedWeatherDataNoCall[,c("Date", "Recorder", "Hour", "PRECTOTCORR", "T2M", "Count")])

# Group by Date and Recorder, then summarize
CombinedWeatherData_grouped <- CombinedWeatherData_all %>%
  group_by(Date, Recorder) %>%
  summarise(
    total_precipitation = sum(PRECTOTCORR, na.rm = TRUE),
    avg_temperature = mean(T2M, na.rm = TRUE),
    total_count = sum(Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Recorder, Date) %>%
  group_by(Recorder) %>%
  mutate(Rainfall_Yesterday = lag(total_precipitation, order_by = Date)) %>%
  mutate(Temp_Yesterday = lag(avg_temperature, order_by = Date)) %>%
  ungroup()

# Convert call counts to binary
CombinedWeatherData_grouped$n.detection <- ifelse(CombinedWeatherData_grouped$total_count == 0, 0, 1)

# Remove NA rows
CombinedWeatherData_grouped <- na.omit(CombinedWeatherData_grouped)

# Extract month and categorize season
CombinedWeatherData_grouped$Month <- as.numeric(substr(CombinedWeatherData_grouped$Date, 6, 7))
CombinedWeatherData_grouped$Season <- ifelse(CombinedWeatherData_grouped$Month > 4 & CombinedWeatherData_grouped$Month < 11, 'Monsoon', 'Dry')
CombinedWeatherData_grouped$Month <- as.factor(CombinedWeatherData_grouped$Month)

# Load GPS data and add to the data frame
TempFrame <- gpx::read_gpx('/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/GPSData/Jahootraining_Bioacoustic Units.gpx')
TempFrame$waypoints$Name <- str_split_fixed(TempFrame$waypoints$Name, pattern = 'UNIT ', n = 2)[,2]
GPSPoints <- data.frame(TempFrame$waypoints$Name, TempFrame$waypoints$Latitude, TempFrame$waypoints$Longitude)
colnames(GPSPoints) <- c('Recorder', 'Latitude', 'Longitude')

# Merge GPS data with the weather and detection data
CombinedWeatherData_alladdgps <- merge(CombinedWeatherData_grouped, GPSPoints, by.x = "Recorder", by.y = "Recorder")

# Convert Date to factor
CombinedWeatherData_alladdgps$Date <- as.factor(CombinedWeatherData_alladdgps$Date)

# Convert recorder to factor
CombinedWeatherData_alladdgps$Recorder <- as.factor(CombinedWeatherData_alladdgps$Recorder)

# View final data frame
head(CombinedWeatherData_alladdgps)
nrow(CombinedWeatherData_alladdgps)
range(as.Date(CombinedWeatherData_alladdgps$Date))

write.csv(CombinedWeatherData_alladdgps,'data/CombinedWeatherData_alladdgps.csv', row.names = F)
# Modeling ----------------------------------------------------------------

# Load required libraries
library(glmmTMB)  # For generalized linear mixed models
library(sf)  # For spatial data handling
library(dplyr)  # For data manipulation
library(DHARMa)  # For residual diagnostics
library(performance)  # For model diagnostics
library(bbmle)  # For model comparison (AIC)
library(sjPlot)  # For model visualization
library(ggeffects)  # For prediction visualization
library(gratia)  # For smooth term visualization

# Define the models
# 1. Intercept model (no predictors)
CrestedGibbonTemporal.intercept <- glmmTMB(
  n.detection ~ (1 | Date/Recorder),  # Random effect of Date/Recorder
  data = CombinedWeatherData_alladdgps,
  family = "binomial"
)

# 2. Model with temperature and rainfall from yesterday
CrestedGibbonTemporal.1 <- glmmTMB(
  n.detection ~ Temp_Yesterday + Rainfall_Yesterday + (1 | Date/Recorder),
  data = CombinedWeatherData_alladdgps,
  family = "binomial"
)

# 3. Model with temperature, precipitation, and season
CrestedGibbonTemporal.2 <- glmmTMB(
  n.detection ~ avg_temperature + total_precipitation + Season + (1 | Date/Recorder),
  data = CombinedWeatherData_alladdgps,
  family = "binomial"
)

# 4. Model with season only
CrestedGibbonTemporal.3 <- glmmTMB(
  n.detection ~ Season + (1 | Date/Recorder),
  data = CombinedWeatherData_alladdgps,
  family = "binomial"
)

# 5. Model with season and temperature
CrestedGibbonTemporal.4 <- glmmTMB(
  n.detection ~ Season + avg_temperature + (1 | Date/Recorder),
  data = CombinedWeatherData_alladdgps,
  family = "binomial"
)

# Test for spatial autocorrelation using DHARMa
groupLocations <- aggregate(CombinedWeatherData_alladdgps[, 11:12], list(CombinedWeatherData_alladdgps$Recorder), mean)
sims <- DHARMa::simulateResiduals(CrestedGibbonTemporal.4)
res2 <- DHARMa::recalculateResiduals(sims, group = CombinedWeatherData_alladdgps$Recorder)
DHARMa::testSpatialAutocorrelation(res2, groupLocations$Latitude, groupLocations$Longitude, plot = FALSE)

# 5. Generalized additive model (GAM) with spatial smooth terms for latitude/longitude and date as a random effect
CrestedGibbonTemporal.5 <- gam(
  n.detection ~ Season + avg_temperature + s(Longitude, Latitude, k = 10, fx = TRUE) +
  s(Date, bs = 're') + s(Recorder, bs = 're'),
  family = binomial,
  data = CombinedWeatherData_alladdgps
)

# 6. GAM with no random effect for date
CrestedGibbonTemporal.5.nore <- gam(
  n.detection ~ Season + avg_temperature + s(Longitude, Latitude, k = 10, fx = TRUE)+ s(Recorder, bs = 're'),
  family = binomial,
  data = CombinedWeatherData_alladdgps
)

# 7. GAM with different spatial smoothness (k = 5) and random effect for date
CrestedGibbonTemporal.6 <- gam(
  n.detection ~ Season + avg_temperature + s(Longitude, Latitude, k = 5, fx = TRUE) + s(Date, bs = 're')+ s(Recorder, bs = 're'),
  family = binomial,
  data = CombinedWeatherData_alladdgps
)

# 8. GAM with only spatial smoothness and date as a random effect
CrestedGibbonTemporal.7 <- gam(
  n.detection ~ Season + s(Longitude, Latitude, k = 10, fx = TRUE) + s(Date, bs = 're')+ s(Recorder, bs = 're'),
  family = binomial,
  data = CombinedWeatherData_alladdgps
)

# 8.full model
CrestedGibbonTemporal.8 <- gam(
  n.detection ~ Season +avg_temperature + total_precipitation+
  Temp_Yesterday + Rainfall_Yesterday+s(Longitude, Latitude, k = 10, fx = TRUE) +
  s(Date, bs = 're')+ s(Recorder, bs = 're'),
  family = binomial,
  data = CombinedWeatherData_alladdgps
)

# Simulate residuals for the top model (CrestedGibbonTemporal.8)
simulationOutput <- simulateResiduals(CrestedGibbonTemporal.8)
plot(simulationOutput)

# Compare models using AIC (Akaike Information Criterion)
bbmle::AICctab(
  CrestedGibbonTemporal.intercept,
  CrestedGibbonTemporal.1,
  CrestedGibbonTemporal.2,
  CrestedGibbonTemporal.3,
  CrestedGibbonTemporal.4,
  CrestedGibbonTemporal.5,
  CrestedGibbonTemporal.6,
  CrestedGibbonTemporal.6.nore,
  CrestedGibbonTemporal.7,
  CrestedGibbonTemporal.8,
  weights = TRUE
)

bbmle::AICctab(
  CrestedGibbonTemporal.intercept,
  CrestedGibbonTemporal.1,
  CrestedGibbonTemporal.2,
  CrestedGibbonTemporal.3,
  CrestedGibbonTemporal.4,
  #CrestedGibbonTemporal.5,
  #CrestedGibbonTemporal.6,
  CrestedGibbonTemporal.6.nore,
  CrestedGibbonTemporal.7,
  CrestedGibbonTemporal.8,
  weights = TRUE
)

sjPlot::tab_model(CrestedGibbonTemporal.intercept,
                  CrestedGibbonTemporal.7,
                  CrestedGibbonTemporal.8,
                  show.aicc = TRUE,show.stat = TRUE)

# Check multicollinearity for the GAM model (CrestedGibbonTemporal.5)
check_collinearity(CrestedGibbonTemporal.7)


# Summary of the top model (CrestedGibbonTemporal.5)
summary(CrestedGibbonTemporal.5)

# Evaluate performance using pseudo-R squared
MuMIn::r.squaredGLMM(CrestedGibbonTemporal.7)

# Check for normality of residuals
check_residuals(simulate_residuals(CrestedGibbonTemporal.7))

# Plot model estimates for fixed effects
Coefplot <- sjPlot::plot_model(CrestedGibbonTemporal.7,
                               type = c("est"), sort.est = TRUE,
                               vline.color = "red", #show.intercept = TRUE,
                   show.values = TRUE, show.p = FALSE, title = "Crested gibbon calls (24-hr)") + theme_bw()

Coefplot <- Coefplot +
  scale_x_discrete(labels = c("Season \n [Monsoon]"))

                     # c("Season \n [Monsoon]", "Temperature \n[current day]", "Rainfall \n [previous day]",
                     #          "Rainfall \n [current day]","Temperature \n [previous day]"))


data_points <- data.frame(
  Latitude = unique(CombinedWeatherData_alladdgps$Latitude),
  Longitude = unique(CombinedWeatherData_alladdgps$Longitude),
  Label = unique(CombinedWeatherData_alladdgps$Recorder) # Example column for labels
)

# Visualize spatial effect of Latitude and Longitude
SpatialPlot <- draw(CrestedGibbonTemporal.5,
     select = 1,   # Selects the first smooth term (i.e., s(Longitude, Latitude))
     rug = TRUE,   # Adds a rug plot to show the data points
     contour = FALSE,
     crs = "+proj=utm +zone=48 +datum=WGS84",
     main = "Spatial Effect of Latitude and Longitude on Detection Probability")# + coord_flip()

SpatialPlot <- SpatialPlot +geom_text(
  aes(y = Latitude, x = Longitude, label = Label),
  data = data_points,  # Ensure this data frame contains the coordinates you want
  size = 4, color = "red",
  vjust = -1, hjust = 0.5  # Adjust positioning as needed
) +ggtitle('')

CombinedModelingPlot <- Coefplot | SpatialPlot

CombinedModelingPlot+ plot_annotation(
  tag_levels = "A"  # Automatically labels subplots as A, B, C, etc.
) &
  theme(plot.tag.position  = c(.935, 1))+ plot_layout(heights = c(0.5, 2))



