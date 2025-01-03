# Randomization experiment
# Question: For each recorder, if I randomly select a start date, how long does it take to get a detection?
# Does this change by recorder?

# Load necessary libraries
library(dplyr)

# Set random seed for reproducibility
set.seed(42)

# Extract the unique recorders and set parameters for the randomization
UniqueRecorders <- unique(CombinedWeatherData_all$Recorder)
num_iterations <- 50
num_randomizations <- 5
ndays <- c(2,4,8,12,24,48)  # Range of consecutive days to test

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

# Summarize the results by recorder, randomization, and number of days
gibbon_detection_count <- gibbon_detection_count %>%
  group_by(Recorder, nrandom, ndays) %>%
  summarise(
    total_count = n(),
    detection_count = sum(SumDetections == 1),
    proportion = detection_count / total_count
  )

# Convert ndays to a factor for plotting
gibbon_detection_count$ndays <- as.factor(gibbon_detection_count$ndays)

# Aggregate data
aggregated_data <- gibbon_detection_count %>%
  group_by(ndays, Recorder) %>%
  summarise(
    mean_proportion = mean(proportion, na.rm = TRUE),
    se_proportion = sd(proportion, na.rm = TRUE) / sqrt(n())
  )

library(ggplot2)

ggplot(data = aggregated_data, aes(fill = factor(ndays), y = mean_proportion, x = Recorder)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean_proportion - se_proportion, ymax = mean_proportion + se_proportion),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(
    title = "Proportion of Detections by Recorder and Number of Days",
    x = "Number of Consecutive Days",
    y = "Mean Proportion of Detections"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
