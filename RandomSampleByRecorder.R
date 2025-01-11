# Randomization experiment
# Question: For each recorder, if I randomly select a start date, how long does it take to get a detection?
# Does this change by recorder?

# Load necessary libraries
library(dplyr)

# Set random seed for reproducibility
set.seed(42)

# Extract the unique recorders and set parameters for the randomization
UniqueRecorders <- unique(CombinedWeatherData_all$Recorder)
num_iterations <- 25
num_randomizations <- 5
ndays <- c(2:21)  # Range of consecutive days to test

# Initialize an empty data frame to store the results
gibbon_detection_count <- data.frame()

# Iterate over the randomizations
for (z in 1:num_randomizations) {

  # Iterate over each recorder
  for (a in 1:length(UniqueRecorders)) {
    print(a)
    # Subset data for the current recorder
    CombinedWeatherData_single <- subset(CombinedWeatherData_all, Recorder == UniqueRecorders[a])

    # Get unique, sorted dates
    UniqueDates <- sort(unique(CombinedWeatherData_single$Date))

    # Calculate the gaps between consecutive dates
    gaps <- c(NA, diff(UniqueDates))
    Dateswithgaps <- data.frame(UniqueDates, gaps)

    # Identify consecutive dates (difference of 1 day)
    Dateswithgaps$Consecutive <- c(FALSE, diff(Dateswithgaps$UniqueDates) == 1)

    # Create the Group column to separate non-consecutive date groups
    Dateswithgaps$Group <- cumsum(!Dateswithgaps$Consecutive)

    # Group the dates based on the Group column
    date_groups <- split(Dateswithgaps$UniqueDates, Dateswithgaps$Group)

    # Filter for groups with at least the minimum number of consecutive dates (ndays)
    long_sequences <- Filter(function(x) length(x) >= min(ndays), date_groups)

    if (length(long_sequences) == 0) {
      next  # Skip to the next iteration if no valid sequences exist
    }

    # Iterate over different day ranges (2 to 10)
    for (b in 1:length(ndays)) {

      # Run the simulation for a set number of iterations
      for (i in 1:num_iterations) {

        # Randomly select a sequence and sample 5 consecutive dates from it
        sequence <- long_sequences[[sample(length(long_sequences), 1)]]
        Random_sample <- sample(length(sequence), 1)
        Dates <- Random_sample:(Random_sample + ndays[b])
        selected_dates <- sequence[Dates]

        # Subset the data based on the selected dates
        filtered_data <- CombinedWeatherData_single %>%
          filter(Date %in% selected_dates)

        # Calculate the number of detections in the selected dates
        SumDetections <- sum(filtered_data$Count)

        # Create a temporary row with detection information
        TempRow <- filtered_data[1,]
        TempRow$SumDetections <- ifelse(SumDetections > 0, 1, 0)
        TempRow$ndays <- ndays[b]
        TempRow$nrandom <- num_randomizations[z]

        # Append the result to the gibbon_detection_count data frame
        gibbon_detection_count <- rbind(gibbon_detection_count, TempRow)
      }
    }
  }
}

gibbon_detection_count$Month <- as.numeric(substr(gibbon_detection_count$Date,6,7))
gibbon_detection_count$Year <- as.numeric(substr(gibbon_detection_count$Date,1,4))

gibbon_detection_count$Season <- ifelse(gibbon_detection_count$Month > 5 & gibbon_detection_count$Month < 11, 'Monsoon', 'Dry')




# Summarize the results by recorder, randomization, and number of days
gibbon_detection_count_proportion <- gibbon_detection_count %>%
  group_by(Year,Season,Recorder, nrandom, ndays) %>%
  summarise(
    total_count = n(),
    detection_count = sum(SumDetections == 1),
    proportion = detection_count / total_count
  )

# Convert ndays to a factor for plotting
gibbon_detection_count_proportion$ndays <- as.factor(gibbon_detection_count_proportion$ndays)

# Aggregate data
aggregated_data <- gibbon_detection_count_proportion %>%
  group_by(Season,Recorder,ndays ) %>%
  summarise(
    mean_proportion = median(proportion, na.rm = TRUE),
    se_proportion = sd(proportion, na.rm = TRUE) / sqrt(n())
  )

head(aggregated_data)
library(ggplot2)

SurveyPlot <- ggplot(data = aggregated_data, aes(fill = factor(ndays), y = mean_proportion, x = Recorder)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean_proportion - se_proportion, ymax = mean_proportion + se_proportion),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(
    x = "Recording Unit",
    y = "Proportion of Randomizations \n with Gibbon Detections"
  ) +
  theme_minimal() + scale_fill_manual(values=matlab::jet.colors(20))+
  theme(legend.position = "top")+ guides(fill = guide_legend(title = "Survey Days"))

facet(SurveyPlot,facet.by = 'Season',nrow = 6)


# Modeling ----------------------------------------------------------------
gibbon_detection_count_proportion$ndays_numer <- as.numeric(as.character(gibbon_detection_count_proportion$ndays))
gibbon_detection_count_proportion$Year <- as.factor(gibbon_detection_count_proportion$Year)
gibbon_detection_count_proportion$Season <- as.factor(gibbon_detection_count_proportion$Season)

gibbon_detection_count_proportion$proportion <-
  pmax(pmin(gibbon_detection_count_proportion$proportion, 1 - .Machine$double.eps), .Machine$double.eps)

# Standardize continuous predictors (e.g., ndays_numer)
#gibbon_detection_count_proportion$ndays_numer <- scale(gibbon_detection_count_proportion$ndays_numer)

gibbon_detection_count_proportion$Season <- factor(gibbon_detection_count_proportion$Season, levels = c( "Monsoon","Dry"))

head(gibbon_detection_count_proportion)

gibbon_detection_count$ndays <- as.numeric(gibbon_detection_count$ndays)

# Fit the model
gibbon_model_null <- glmmTMB(
  SumDetections ~ (1 | Recorder),  # Predictor: Season, random effect: Recorder
  data = gibbon_detection_count,
  family = binomial()
)

# Fit the model
gibbon_model <- glmmTMB(
  SumDetections ~ Season + ndays  +(1 | Recorder),  # Predictor: Season, random effect: Recorder
  data = gibbon_detection_count,
  family =  binomial()
)

gibbon_model_randomslope <- glmmTMB(
  SumDetections ~ Season + ndays + (1 + ndays | Recorder),  # Random slope for ndays_numer
  data = gibbon_detection_count,
  family = binomial()
)

bbmle::AICctab(gibbon_model_null,gibbon_model,gibbon_model_randomslope, weights=T)


# Fit the model
gibbon_model_null <- glmmTMB(
  proportion ~ (1 | Recorder),  # Predictor: Season, random effect: Recorder
  data = gibbon_detection_count_proportion,
  family = beta_family()
)

# Fit the model
gibbon_model <- glmmTMB(
  proportion ~ Season + ndays_numer  +(1 | Recorder),  # Predictor: Season, random effect: Recorder
  data = gibbon_detection_count_proportion,
  family = beta_family()
)

gibbon_model_randomslope <- glmmTMB(
  proportion ~ Season + ndays_numer + (1 + ndays_numer | Recorder),  # Random slope for ndays_numer
  data = gibbon_detection_count_proportion,
  family = beta_family()
)


# Load GPS data and add to the data frame
TempFrame <- gpx::read_gpx('/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/GPSData/Jahootraining_Bioacoustic Units.gpx')
TempFrame$waypoints$Name <- str_split_fixed(TempFrame$waypoints$Name, pattern = 'UNIT ', n = 2)[,2]
GPSPoints <- data.frame(TempFrame$waypoints$Name, TempFrame$waypoints$Latitude, TempFrame$waypoints$Longitude)
colnames(GPSPoints) <- c('Recorder', 'Latitude', 'Longitude')

# Merge GPS data with the weather and detection data
gibbon_detection_count_proportion_addgps <- merge(gibbon_detection_count_proportion, GPSPoints, by.x = "Recorder", by.y = "Recorder")
gibbon_detection_count_proportion_addgps$Recorder <-as.factor(gibbon_detection_count_proportion_addgps$Recorder)

# Test for spatial autocorrelation using DHARMa
groupLocations <- aggregate(gibbon_detection_count_proportion_addgps[, 10:11], list(gibbon_detection_count_proportion_addgps$Recorder), mean)
sims <- DHARMa::simulateResiduals(gibbon_model_randomslope)
res2 <- DHARMa::recalculateResiduals(sims, group = gibbon_detection_count_proportion_addgps$Recorder)
DHARMa::testSpatialAutocorrelation(res2, groupLocations$Latitude, groupLocations$Longitude, plot = FALSE)


gibbon_model_spatial <- gam(
  proportion ~ Season + ndays_numer + s(Latitude, Longitude, k = 10) + s(Recorder, bs = "re"),
  family = betar(link = "logit"),  # Beta regression for proportion data
  data = gibbon_detection_count_proportion_addgps
)

bbmle::AICctab(gibbon_model_null,gibbon_model,gibbon_model_randomslope, weights=T)

# Summarize the model
summary(gibbon_model_randomslope)

sjPlot::plot_model(gibbon_model_randomslope,type=c("est"),
                   #terms=c('Season','ndays_numer'),
                   sort.est = TRUE, vline.color = "red",
                   show.values = TRUE,
                   show.p = FALSE,
                   title = "Crested gibbon calls (24-hr)")+theme_bw()+ylab('Detection probability')

sjPlot::plot_model(gibbon_model_randomslope,type=c("eff"),
                   terms=c('ndays','Season'),
                   sort.est = TRUE,
                   vline.color = "red",
                   show.values = TRUE,
                   show.p = FALSE,
                   title = "Crested gibbon calls \n (binary in a 24-hr period)")+
                   theme_bw()+ylab('Detection probability')+xlab('Number of recording days')


sjPlot::plot_model(gibbon_model,type=c("re"),
                   #terms=c('ndays_numer','Season'),
                   sort.est = TRUE,
                   vline.color = "red",
                   show.values = TRUE,
                   show.p = FALSE,
                   title = "Crested gibbon calls \n (binary in a 24-hr period)")+
  theme_bw()+ylab('Detection probability')+xlab('Recorder')

# Define the logit function
logit <- function(p) {
  log(p / (1 - p))
}

# Extract fixed effects from the model
coefficients <- fixef(gibbon_model_randomslope)

# Example for Dry season (or you can use another season, e.g., "Monsoon")
season_effect_dry <- coefficients$cond[2]    # Intercept term (for Dry season)
ndays_effect <- coefficients$cond[3]  # Coefficient for ndays_numer

# Set the target proportion (e.g., when proportion reaches 0.95 or close to 1)
target_proportion <- 0.95

# Solve the equation for ndays_numer
ndays_for_target <- (logit(target_proportion) - season_effect_dry) / ndays_effect

# Print the result
ndays_for_target

# SeasonDry
# 45.51478

# SeasonMonsoon
# 64.27947
