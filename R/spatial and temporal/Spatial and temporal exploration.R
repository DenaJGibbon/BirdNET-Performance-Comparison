# Load required libraries
library(stringr)  # String manipulation
library(ggpubr)   # ggplot2 extensions for easy plotting
library(gpx)      # Reading GPX files
library(sp)       # Spatial data handling
library(gstat)    # Interpolation and geostatistical modeling
library(matlab)   # Color palettes (e.g., jet.colors)
library(ggplot2)  # Data visualization
library(gridExtra)


# Load detection data from a CSV file
DetectionsDF <- read.csv('/Users/denaclink/Downloads/verified_detections.csv')  # Load verified detection data
head(DetectionsDF)                                       # Display the first few rows

# Load GPS data and format it
TempFrame <- gpx::read_gpx('/Users/denaclink/Desktop/RStudio Projects/Cambodia-Data-Analysis/Selection Table Data/Bioacoustic Units.gpx')  # Load GPS data
TempFrame$waypoints$Name <- str_split_fixed(TempFrame$waypoints$Name, pattern = 'UNIT ', n = 2)[, 2]  # Extract recorder names
GPSPoints <- data.frame(TempFrame$waypoints$Name, TempFrame$waypoints$Latitude, TempFrame$waypoints$Longitude)  # Create data frame
colnames(GPSPoints) <- c('Recorder', 'Latitude', 'Longitude')  # Rename columns
head(GPSPoints)                                                # Display the first few rows

# Merge detection data with GPS data
CombinedDetectionData <- merge(DetectionsDF, GPSPoints, by.x = "Recorder", by.y = "Recorder")  # Merge datasets
head(CombinedDetectionData)  # Display the first few rows

CombinedDetectionData <- CombinedDetectionData[, !colnames(CombinedDetectionData) %in% 'X']

CombinedDetectionData$Date <- as.Date(CombinedDetectionData$Date, format = "%Y-%m-%d")

# Visualize the hourly distribution of detections
ggpubr::gghistogram(data = CombinedDetectionData, x = 'Hour', stat = "count") +
  ylab('True positive detections')  # Create a histogram

ggpubr::gghistogram(data = CombinedDetectionData, x = 'Date', stat = "count") +
  ylab('True positive detections')  # Create a histogram


# Process detection files that don't contain calls
JahooFilesBase <- list.files("/Users/denaclink/Downloads/JahooBirdNETupdatedmodel", pattern = '.txt', recursive = TRUE, full.names = FALSE)  # List detection files
JahooFilesBase <- str_split_fixed(basename(JahooFilesBase), pattern = '.BirdNET', n = 2)[, 1]  # Format file names
JahooFilesBaseShort <- substr(JahooFilesBase, 1, 17)  # Extract a short version of file names

# Match detection dates and times
DateMatch <- str_remove_all(DetectionsDF$Date, '-')  # Remove dashes from dates
HourPad <- str_pad(DetectionsDF$Hour, 2, pad = "0")  # Pad hours to 2 digits
TruePositiveDetectionsName <- paste(DetectionsDF$Recorder, DateMatch, HourPad, sep = '_')  # Create unique names

# Identify files without calls
JahooFilesBaseNocall <- JahooFilesBase[-which(JahooFilesBaseShort %in% TruePositiveDetectionsName)]  # Exclude files with true detections
JahooFilesBaseNocallSplit <- str_split_fixed(JahooFilesBaseNocall, pattern = '_', n = 4)  # Split file names into components

# Create a data frame for files without calls
Recorder <- JahooFilesBaseNocallSplit[, 1]
Date <- as.Date(JahooFilesBaseNocallSplit[, 2], format = "%Y%m%d")
Month <- as.numeric(substr(Date, 6, 7))
Hour <- substr(JahooFilesBaseNocallSplit[, 3], 1, 2)
JahooNoCallDF <- data.frame(Recorder, Date, Month, Hour)  # Create a data frame
head(JahooNoCallDF)  # Display the first few rows

# Analyze the distribution of non-call detections by hour
table(JahooNoCallDF$Hour)

# Merge detection data with GPS data
JahooNoCallDF <- merge(JahooNoCallDF, GPSPoints, by.x = "Recorder", by.y = "Recorder")  # Merge datasets
head(JahooNoCallDF)  # Display the first few rows

CombinedDetectionData$GibbonDetect <- '1'
JahooNoCallDF$GibbonDetect <- '0'

CombinedCallNoCallDF <- rbind.data.frame(CombinedDetectionData,JahooNoCallDF)

CombinedCallNoCallDF$GibbonDetect <- as.numeric(CombinedCallNoCallDF$GibbonDetect)

head(CombinedCallNoCallDF)

library(dplyr)
library(ggplot2)

# Define the monsoon period (May to October) for the years in your dataset
monsoon_start_1 <- as.Date("2022-07-01")
monsoon_end_1 <- as.Date("2022-9-30")
monsoon_start_2 <- as.Date("2023-07-01")
monsoon_end_2 <- as.Date("2023-9-30")

# Step 1: Summarize data
# Step 1: Summarize data
summary_df <- CombinedCallNoCallDF %>%
  group_by(Date) %>%
  summarize(
    TotalRecorders = n(),
    Detections = sum(GibbonDetect),
    ProportionDetected = Detections / TotalRecorders
  ) %>%
  filter(Date > as.Date("2022-02-28"))  # Exclude data before March 2022


# Step 2: Plot the proportion over time
ggplot(summary_df, aes(x = as.Date(Date), y = ProportionDetected)) +
  geom_line(color = "grey", size = 1) +
  #geom_point(color = "red", size = 2) +
  labs(
    #title = "Proportion of Recorders with Detections Over Time",
    x = "Date",
    y = "Proportion of Recorders with Detections"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  geom_smooth(method = "gam", color = "darkgreen", fill = "lightgreen", size = 1.2, alpha = 0.3) +  # Smoothed trend line
  # Add shaded area for the monsoon period across multiple years
  geom_rect(aes(xmin = monsoon_start_1, xmax = monsoon_end_1, ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.01) +  # Light blue shading for 2022
  geom_rect(aes(xmin = monsoon_start_2, xmax = monsoon_end_2, ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.01)    # Light blue shading for 2023



# Interpolate call density based on GPS data
recorder.index <- unique(CombinedCallNoCallDF$Recorder)  # Get unique recorders

interpolation.df <- data.frame()                         # Initialize data frame for interpolation

for (x in 1:length(recorder.index)) {
  index.sub <- recorder.index[x]                                     # Subset recorder
  gps.sub <- subset(CombinedCallNoCallDF, Recorder == as.character(index.sub))  # Subset GPS data
  new.df <- cbind.data.frame(gps.sub$Recorder[1], gps.sub$Latitude[1],
                             gps.sub$Longitude[1], sum(gps.sub$GibbonDetect))  # Create a new row
  colnames(new.df) <- c("name", "y", "x", "n.detect")               # Rename columns

  TempRecRow <- subset(JahooNoCallDF, Recorder == index.sub)        # Subset non-call data
  Nuniquerecordings <- nrow(gps.sub)

  new.df$n.detect <- new.df$n.detect / Nuniquerecordings  # Normalize detections

  interpolation.df <- rbind.data.frame(interpolation.df, new.df)   # Append to interpolation data frame
}

# Prepare data for interpolation and plotting
interpolation.df.for.pts <- interpolation.df
x.range <- as.numeric(c(min(interpolation.df$x), max(interpolation.df$x)))  # Set longitude range
y.range <- as.numeric(c(min(interpolation.df$y), max(interpolation.df$y)))  # Set latitude range
sp::coordinates(interpolation.df) <- ~x + y                                # Set spatial coordinates
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 1e-05),
                   y = seq(from = y.range[1], to = y.range[2], by = 1e-05))  # Create grid
sp::coordinates(grd) <- ~x + y                                              # Set grid coordinates
sp::gridded(grd) <- TRUE                                                    # Make grid spatial

# Perform interpolation using inverse distance weighting (IDW)
idw <- gstat::idw(formula = n.detect ~ 1, locations = interpolation.df, newdata = grd)  # Perform IDW interpolation
idw.output <- as.data.frame(idw)                                           # Convert to data frame
names(idw.output)[1:3] <- c("long", "lat", "var1.pred")                    # Rename columns

# Generate color palette
vid.cols <- viridis::viridis (10)

# Plot call density
Jahoo.call.density.plot <- ggplot2::ggplot() +
  geom_tile(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) +  # Add interpolated data
  geom_label(data = interpolation.df.for.pts, mapping = aes(x = x, y = y, label = name), size = 4) +  # Add labels
  scale_fill_gradientn(colors = vid.cols) +                                # Set color gradient
  xlab("Longitude") + ylab("Latitude") +                                   # Set axis labels
  theme_bw() +                                                             # Set theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # Remove grid lines
  guides(fill = guide_legend(title = "Jahoo detections \n per hour")) +    # Add legend
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))  # Style legend

Jahoo.call.density.plot  # Display plot


# Add monthly -------------------------------------------------------------

# Open PDF device to combine all plots in a single file
output_pdf <- paste("Combined_Plots_multipanel.pdf")
pdf(output_pdf, height = 24, width = 18)  # Set height and width to fit two plots per row

# List to store ggplot objects
plot_list <- list()

# Interpolate call density based on GPS data
recorder.index <- unique(CombinedCallNoCallDF$Recorder)  # Get unique recorders
month.index <- sort(unique(CombinedCallNoCallDF$Month))

for(w in c(3:length(month.index))){
  CombinedCallNoCallDFSub <- subset(CombinedCallNoCallDF, Month == month.index[w])

  interpolation.df <- data.frame()  # Initialize data frame for interpolation

  for (x in 1:length(recorder.index)) {
    index.sub <- recorder.index[x]  # Subset recorder
    gps.sub <- subset(CombinedCallNoCallDFSub, Recorder == as.character(index.sub))  # Subset GPS data
    new.df <- cbind.data.frame(gps.sub$Recorder[1], gps.sub$Latitude[1],
                               gps.sub$Longitude[1], sum(gps.sub$GibbonDetect))  # Create a new row
    colnames(new.df) <- c("name", "y", "x", "n.detect")  # Rename columns

    TempRecRow <- subset(JahooNoCallDF, Recorder == index.sub)  # Subset non-call data
    Nuniquerecordings <- nrow(gps.sub)

    new.df$n.detect <- new.df$n.detect / Nuniquerecordings  # Normalize detections

    interpolation.df <- rbind.data.frame(interpolation.df, new.df)  # Append to interpolation data frame
  }

  interpolation.df <- na.omit(interpolation.df)

  #if (nrow(interpolation.df) == 10) {
    # Prepare data for interpolation and plotting
    interpolation.df.for.pts <- interpolation.df
    x.range <- as.numeric(c(min(interpolation.df$x), max(interpolation.df$x)))  # Set longitude range
    y.range <- as.numeric(c(min(interpolation.df$y), max(interpolation.df$y)))  # Set latitude range
    sp::coordinates(interpolation.df) <- ~x + y  # Set spatial coordinates
    grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 1e-05),
                       y = seq(from = y.range[1], to = y.range[2], by = 1e-05))  # Create grid
    sp::coordinates(grd) <- ~x + y  # Set grid coordinates
    sp::gridded(grd) <- TRUE  # Make grid spatial

    # Perform interpolation using inverse distance weighting (IDW)
    idw <- gstat::idw(formula = n.detect ~ 1, locations = interpolation.df, newdata = grd)  # Perform IDW interpolation
    idw.output <- as.data.frame(idw)  # Convert to data frame
    names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # Rename columns

    # Define value ranges and corresponding colors
    breaks <- seq(0, 0.3, length.out = 6)  # Adjust the number of breaks if needed
    # Example breaks within your range
    colors <- c("blue", "green", "yellow", "red", "white")  # Corresponding colors for the ranges

    # Set the same color scale across all plots
    color_scale <- scale_fill_gradientn(
      colors = colors,
      values = scales::rescale(breaks),
      limits = c(0.0, 0.3),  # Set the limits to the full range of values
      breaks = breaks  # Define the breaks for the color scale
    )

    # Create ggplot object for each month and store it in the list
    p <- ggplot2::ggplot() +
      geom_tile(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) +  # Add interpolated data
      geom_point(data = interpolation.df.for.pts, mapping = aes(x = x, y = y), color = "black", size = 1) +  # Add points instead of labels
      color_scale +  # Apply the fixed color scale
      xlab("Longitude") + ylab("Latitude") +  # Set axis labels
      theme_bw() +  # Set theme
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # Remove grid lines
      guides(fill = guide_legend(title = "Jahoo detections \n per hour")) +  # Add legend
      ggtitle(month.index[w]) +  # Add title with the month name
      theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))  # Style legend

    plot_list[[w]] <- p  # Add the plot to the list
  }
#}

plot_list <- plot_list[!sapply(plot_list, is.null)]

# Combine all plots into a 2xN grid
grid.arrange(grobs = plot_list, ncol = 2)

# Close the PDF device after all plots are added
dev.off()

