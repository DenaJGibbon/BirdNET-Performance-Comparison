rain_data <- get_power(
  community = "RE",
  lonlat = c(107.106802, 12.357878),
  dates = c("2022-03-13", "2024-04-09"),
  temporal_api = "DAILY",
  pars = c("PRECTOTCORR","T2M")
)
# Modeling ----------------------------------------------------------------
library(glmmTMB)
library(performance)

JahooFilesBase <- list.files("/Users/denaclink/Desktop/RStudioProjects/BirdNET-Performance-Comparison/data/JahooBirdNETupdatedmodel", pattern = '.txt', recursive=T, full.names=F)
JahooFilesBase <- basename(JahooFilesBase)
JahooFilesBase <- str_split_fixed(JahooFilesBase,pattern = '.BirdNET',n=2)[,1]
length(JahooFilesBase) # 130686

TruePositiveDetections <- list.files('/Volumes/DJC Files/JahooGibbonBirdNETDetections/CrestedGibbons/Wav/Positive/')
TruePositiveDetectionsShort <- str_split_fixed(TruePositiveDetections,pattern = '_R',n=2)[,2]
TruePositiveDetectionsShort <- str_split_fixed(TruePositiveDetectionsShort,pattern = '_.wav',n=2)[,1]
TruePositiveDetectionsName <- paste('R',TruePositiveDetectionsShort,sep='')

JahooFilesBaseNocall <- JahooFilesBase[-which(JahooFilesBase %in%TruePositiveDetectionsName )]
JahooFilesBaseNocallSplit <- str_split_fixed(JahooFilesBaseNocall,pattern = '_',n=4)
Recorder <- JahooFilesBaseNocallSplit[,1]
Date <- JahooFilesBaseNocallSplit[,2]
Date <- as.Date(Date, format = "%Y%m%d")
Month <- as.numeric(substr(Date,6,7))
Hour <- substr(JahooFilesBaseNocallSplit[,3],1,2)
Second <- substr(JahooFilesBaseNocallSplit[,3],5,6)

JahooNoCallDF <- cbind.data.frame(Recorder, Date, Month, Hour,Second)
CombinedWeatherDataNoCall <- merge(x=JahooNoCallDF, y=rain_data, by.x = "Date", by.y = "YYYYMMDD")
CombinedWeatherDataNoCall$Count <- 0

CombinedWeatherData <- merge(x=TruePositiveDF, y=rain_data, by.x = "Date", by.y = "YYYYMMDD")

# Count the number of detections per Recorder, Date, and Hour
CombinedWeatherData_counts <- CombinedWeatherData %>%
  group_by(Date, Recorder, Hour,PRECTOTCORR,T2M) %>%
  summarise(Count = n(), .groups = "drop")  # Count number of rows in each group and remove grouping

# View the result
head(CombinedWeatherData_counts)

CombinedWeatherData_all <- rbind.data.frame(CombinedWeatherData_counts,CombinedWeatherDataNoCall[,c("Date", "Recorder", "Hour", "PRECTOTCORR", "T2M", "Count")])

tail(CombinedWeatherData_all)


# Grouping by Date and Recorder, then summarizing data
library(dplyr)

CombinedWeatherData_grouped <- CombinedWeatherData_all %>%
  group_by(Date, Recorder) %>%
  summarise(
    total_precipitation = sum(PRECTOTCORR, na.rm = TRUE),  # Sum of precipitation
    avg_temperature = mean(T2M, na.rm = TRUE),             # Average temperature
    total_count = sum(Count, na.rm = TRUE),                # Sum of count
    .groups = "drop"                                       # Drop grouping after summarizing
  ) %>%
  arrange(Recorder, Date) %>%  # Ensure data is sorted by Recorder and Date
  group_by(Recorder) %>%
  mutate(Rainfall_Yesterday = lag(total_precipitation, order_by = Date)) %>%  # Use total_precipitation for lag
  mutate(Temp_Yesterday = lag(avg_temperature, order_by = Date)) %>%  # Use total_precipitation for lag
  ungroup()

# View the result
head(CombinedWeatherData_grouped)

# Convert call counts to binary
CombinedWeatherData_grouped$n.detection <-
  ifelse(CombinedWeatherData_grouped$total_count  == 0 , 0, 1)

CombinedWeatherData_grouped <- na.omit(CombinedWeatherData_grouped)

CombinedWeatherData_grouped$Month <- str_split_fixed(CombinedWeatherData_grouped$Date,
                                                     pattern = '-',n=3)[,2]

CombinedWeatherData_grouped$Month <- as.factor(CombinedWeatherData_grouped$Month)

CrestedGibbonTemporal.intercept <-
  glmmTMB(
    n.detection ~  (1 | Date/Recorder),
    data = CombinedWeatherData_grouped,
    family = "binomial"
  )

CrestedGibbonTemporal.1 <-
  glmmTMB(
    n.detection ~ Temp_Yesterday  + Rainfall_Yesterday+ (1 | Date/Recorder),
    data = CombinedWeatherData_grouped,
    family = "binomial"
  )

CrestedGibbonTemporal.2 <-
  glmmTMB(
    n.detection ~ avg_temperature  + total_precipitation+  (1 | Date/Recorder),
    data = CombinedWeatherData_grouped,
    family = "binomial"
  )

CrestedGibbonTemporal.3 <-
  glmmTMB(
    n.detection ~ avg_temperature   + (1 | Date/Recorder),
    data = CombinedWeatherData_grouped,
    family = "binomial"
  )

# Compare models using AIC
bbmle::AICctab(
  CrestedGibbonTemporal.intercept,
  CrestedGibbonTemporal.1,
  CrestedGibbonTemporal.2,
  CrestedGibbonTemporal.3 ,
  weights = T
)

# Check summary of top model
anova(CrestedGibbonTemporal.2, CrestedGibbonTemporal.intercept)

summary(CrestedGibbonTemporal.2)

# Evaluate performance using pseudo-R squared
MuMIn::r.squaredGLMM(CrestedGibbonTemporal.2)

# Check for normality of residuals
check_residuals(simulate_residuals(CrestedGibbonTemporal.3))

sjPlot::plot_model(CrestedGibbonTemporal.2,type=c("est"),
                   sort.est = TRUE, vline.color = "red",
                   show.values = TRUE,
                   show.p = FALSE,
                   title = "Crested gibbon calls (24-hr)")+theme_bw()+ylim(0.5,1.5)

Rainplot <- sjPlot::plot_model(CrestedGibbonTemporal.2,type=c("eff"),
                   terms=c('total_precipitation'),
                   sort.est = TRUE, vline.color = "red",
                   show.values = TRUE,
                   show.p = FALSE,
                   title = "Crested gibbon calls (24-hr)")+theme_bw()

TempPlot <- sjPlot::plot_model(CrestedGibbonTemporal.2,type=c("eff"),
                               terms=c('avg_temperature'),
                               sort.est = TRUE, vline.color = "red",
                               show.values = TRUE,
                               show.p = FALSE,
                               title = "Crested gibbon calls (24-hr)")+theme_bw()


cowplot::plot_grid(Rainplot,TempPlot)

# Plot RE
#sjPlot::plot_model(CrestedGibbonTemporal.2, 're',sort.est = TRUE)
