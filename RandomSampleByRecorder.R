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
ndays <- seq(1:30)# Sequence of consecutive days from 2 to 21, incrementing by 2

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

        # Randomly select a sequence and sample consecutive dates from it
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

gibbon_detection_count$Season <- ifelse(gibbon_detection_count$Month > 4 & gibbon_detection_count$Month < 11, 'Monsoon', 'Dry')

write.csv(gibbon_detection_count,'data/gibbon_detection_count_randomization.csv',row.names = F)

library(dplyr)

# Summarize the results by recorder, randomization, and number of days
gibbon_detection_count_proportion <- gibbon_detection_count %>%
  group_by(Year,Season,Recorder, nrandom, ndays) %>%
  dplyr::summarise(
    total_count = n(),
    detection_count = sum(SumDetections == 1),
    proportion = detection_count / total_count
  )

# Convert ndays to a factor for plotting
gibbon_detection_count_proportion$ndays <- as.factor(gibbon_detection_count_proportion$ndays)


ggerrorplot(data= gibbon_detection_count,
          x='ndays',y='Count',color = 'Recorder',facet.by = 'Season')

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
  # geom_errorbar(aes(ymin = mean_proportion - se_proportion, ymax = mean_proportion + se_proportion),
  #               position = position_dodge(width = 0.8), width = 0.2) +
  labs(
    x = "Recording Unit",
    y = "Proportion of Randomizations \n with Gibbon Detections"
  ) +
  theme_minimal() + scale_fill_manual(values=matlab::jet.colors(30))+
  theme(legend.position = "top")+guides(fill='none')+ylim(0,1)

facet(SurveyPlot,facet.by = 'Season',nrow = 6)


library(ggplot2)
library(dplyr)



# Modeling ----------------------------------------------------------------
library(glmmTMB)
library(DHARMa)
library(performance)
library(bbmle)

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

# Check for normality of residuals
check_residuals(simulate_residuals(gibbon_model_randomslope))

simulationOutput <- simulateResiduals(gibbon_model_randomslope)
plot(simulationOutput)

# Load GPS data and add to the data frame
TempFrame <- gpx::read_gpx('/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/GPSData/Jahootraining_Bioacoustic Units.gpx')
TempFrame$waypoints$Name <- str_split_fixed(TempFrame$waypoints$Name, pattern = 'UNIT ', n = 2)[,2]
GPSPoints <- data.frame(TempFrame$waypoints$Name, TempFrame$waypoints$Latitude, TempFrame$waypoints$Longitude)
colnames(GPSPoints) <- c('Recorder', 'Latitude', 'Longitude')

# Merge GPS data with the weather and detection data
gibbon_detection_count <- merge(gibbon_detection_count, GPSPoints, by.x = "Recorder", by.y = "Recorder")
gibbon_detection_count$Recorder <-as.factor(gibbon_detection_count$Recorder)

# Test for spatial autocorrelation using DHARMa
groupLocations <- aggregate(gibbon_detection_count[, 13:14], list(gibbon_detection_count$Recorder), mean)
sims <- DHARMa::simulateResiduals(gibbon_model_randomslope)
res2 <- DHARMa::recalculateResiduals(sims, group = gibbon_detection_count$Recorder)
DHARMa::testSpatialAutocorrelation(res2, groupLocations$Latitude, groupLocations$Longitude, plot = FALSE)

gibbon_detection_count$Recorder <- as.factor(gibbon_detection_count$Recorder)

gibbon_model_spatial <- gam(
  SumDetections ~ Season + ndays  + s(Longitude, Latitude, k = 5, fx = TRUE) + s(Recorder, bs = 're'),
  family = binomial,
  data = gibbon_detection_count
)


bbmle::AICctab(gibbon_model_null,
               gibbon_model,
               gibbon_model_randomslope,
               gibbon_model_spatial, weights=T)

MuMIn::r.squaredGLMM(gibbon_model_randomslope)


sjPlot::tab_model(gibbon_model_null,gibbon_model,gibbon_model_randomslope,
                  show.aicc = TRUE, show.p = TRUE)

ggpredict(gibbon_model_randomslope, terms = c('ndays','Season'), type="fe") %>% plot()


sjPlot::plot_model(gibbon_model_randomslope,type=c("est"),
                   #terms=c('Season','ndays_numer'),
                   sort.est = TRUE, vline.color = "red",
                   show.values = TRUE,
                   show.p = FALSE,
                   title = "Crested gibbon calls (24-hr)")+theme_bw()+ylab('Detection probability')

EffectsModel <- plot_model(gibbon_model_randomslope,type='eff',terms=c('ndays','Season'))+
  theme_bw()+xlab('Number of survey days')+ylab('Probability of detection')

EffectsModel +
  scale_y_continuous(
    labels = function(x) x * 1 # Transform labels
  )+ggtitle('')

sjPlot::plot_model(gibbon_model_randomslope,type=c('re'))

CombinedModelingPlot <- Coefplot | SpatialPlot

CombinedModelingPlot+ plot_annotation(
  tag_levels = "A"  # Automatically labels subplots as A, B, C, etc.
) &
  theme(plot.tag.position  = c(.935, 1))+ plot_layout(heights = c(1, 2))



# Plot the smooth term for ndays (you can specify which predictor to view by using the argument 'terms')
vis.gam(gibbon_model_spatial)

draw(gibbon_model_spatial,
     select = 1,   # Selects the first smooth term (i.e., s(Longitude, Latitude))
     rug = TRUE,   # Adds a rug plot to show the data points
     contour = FALSE,
     crs = "+proj=utm +zone=48 +datum=WGS84",
     main = "Spatial Effect of Latitude and Longitude on Detection Probability")# + coord_flip()


# Define the logit function
logit <- function(p) {
  log(p / (1 - p))
}

# Switch the order of levels
gibbon_detection_count$Season <- factor(
  gibbon_detection_count$Season,
  levels = c( "Monsoon","Dry")  # Desired order
)

# Verify the new levels
levels(gibbon_detection_count$Season)

gibbon_model_randomslope_switch <- glmmTMB(
  SumDetections ~ Season + ndays + (1 + ndays | Recorder),  # Random slope for ndays_numer
  data = gibbon_detection_count,
  family = binomial()
)
# Extract fixed effects from the model
coefficients <- fixef(gibbon_model_randomslope_switch)

# Example for Dry season (or you can use another season, e.g., "Monsoon")
season_effect_dry <- coefficients[[1]][2]    # Intercept term (for Dry season)
ndays_effect <- coefficients[[1]][3]  # Coefficient for ndays_numer

# Set the target proportion (e.g., when proportion reaches 0.95 or close to 1)
target_proportion <- 0.95

# Solve the equation for ndays_numer
ndays_for_target <- (logit(target_proportion) - season_effect_dry) / ndays_effect

# Print the result
ndays_for_target

# SeasonDry
# 8.672577

# SeasonMonsoon
# 48.781

# Desired probability threshold
p <- 0.95

coefficients <- fixef(gibbon_model_randomslope_switch)

# Extract coefficients from conf.model
intercept <- coefficients$cond[1]
slope <- coefficients$cond [3]

# Calculate threshold
logit_threshold <- (log(p / (1 - p)) - intercept) / slope
probability_threshold <- exp(logit_threshold) / (1 + exp(logit_threshold))

# Print results
logit_threshold




# Random slope figure -----------------------------------------------------

library(ggplot2)
library(glmmTMB)

# Generate new data for predictions
new_data <- expand.grid(
  ndays = seq(min(gibbon_detection_count$ndays), max(gibbon_detection_count$ndays), length.out = 100),
  Season = unique(gibbon_detection_count$Season),
  Recorder = unique(gibbon_detection_count$Recorder)
)

# Predict with random effects and include standard errors
preds <- predict(
  gibbon_model_randomslope,
  newdata = new_data,
  re.form = NULL,  # Include random effects
  type = "response",
  se.fit = TRUE  # Get standard errors for CI calculation
)

# Add predicted values and standard errors to the data frame
new_data$Predicted <- preds$fit
new_data$SE <- preds$se.fit

# Calculate the 95% confidence intervals (CI) using the standard errors
new_data$CI_lower <- new_data$Predicted - 1.96 * new_data$SE
new_data$CI_upper <- new_data$Predicted + 1.96 * new_data$SE

# Plot the observed data and random slopes with CI
ggplot(new_data, aes(x = ndays, y = Predicted, group = Recorder, color = Recorder)) +
  geom_line() +  # Fitted lines for each Recorder
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Recorder), alpha = 0.2) +  # Add confidence intervals
  facet_wrap(~Season) +  # Facet by season if applicable
  theme_minimal() +
  labs(
    x = "Number of Survey Days",
    y = "Probability of a \n gibbon detection",
    color = "Recorder"
  )+ scale_color_brewer(palette = "Set3")+scale_fill_brewer(palette = "Set3")
