# Load necessary libraries
library(stringr)  # For string manipulation
library(caret)    # For machine learning and model evaluation
library(ggpubr)   # For data visualization
library(dplyr)    # For data manipulation
library(data.table) # For sorting the detections
library(ggplot2)
library(ROCR)
library(pROC)

# NOTE you need to change the file paths below to where your files are located on your computer
detect.class <- 'gibbon'

#  Performance Binary --------------------------------------------------------
PerformanceFolders <- list.files('/Volumes/DJC Files/JahooGibbonTestDataPerformanceSmall/',
                                 full.names = TRUE)

# Get a list of annotation selection table files
TestDataSet <-  list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/AnnotatedFiles',
                           full.names = TRUE)

start.time.buffer <- 6
end.time.buffer <- 6

CombinedF1data <- data.frame()

for(z in 1:length(PerformanceFolders)){
  print(paste('processing', z, 'out of', length(PerformanceFolders)))
  # Get a list of TopModel result files
  TopModelresults <- list.files(PerformanceFolders[[z]],
                                full.names = TRUE)

  # Preallocate space for TopModelDetectionDF
  TopModelDetectionDF <- data.frame()

  Seq <- seq(1:length(TopModelresults))

  # Loop through each TopModel result file
  for (a in Seq) {

    # Read the TopModel result table into a data frame
    TempTopModelTable <- read.delim2(TopModelresults[a])

    TempTopModelTable <- TempTopModelTable[,-c(4,5)]

    TempTopModelTable <- subset(TempTopModelTable,Common.Name==detect.class)

    # Extract the short name of the TopModel result file
    ShortName <- basename(TopModelresults[a])
    ShortName <- str_split_fixed(ShortName, pattern = '.BirdNET', n = 2)[, 1]

    # Find the corresponding annotation selection table
    testDataIndex <- which(str_detect(TestDataSet, ShortName))

    if(length(testDataIndex) > 0){
      TestDataTable <- read.delim2(TestDataSet[testDataIndex])

      TestDataTable$Class <- detect.class
      # Round Begin.Time..s. and End.Time..s. columns to numeric
      TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
      TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))

      DetectionList <- list()
      # Loop through each row in TempTopModelTable
      for (c in 1:nrow(TempTopModelTable)) {
        TempRow <- TempTopModelTable[c,]


        # Check if Begin.Time..s. is not NA
        if (!is.na(TempRow$Begin.Time..s.)) {
          # Convert Begin.Time..s. and End.Time..s. to numeric
          TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
          TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)

          # Determine if the time of the detection is within the time range of an annotation
          TimeBetween <- data.table::between(TempRow$Begin.Time..s.,
                                             TestDataTable$Begin.Time..s. - start.time.buffer,
                                             TestDataTable$End.Time..s. + end.time.buffer)


          # Extract the detections matching the time range
          matched_detections <- TestDataTable[TimeBetween, ]

          if (nrow(matched_detections) > 0) {
            # Set Class based on the Call.Type in matched_detections
            TempRow$Class <- detect.class
            DetectionList[[length( unlist(DetectionList))+1]] <-  which(TimeBetween == TRUE)
          } else {
            # Set Class to 'noise' if no corresponding annotation is found
            TempRow$Class <- 'noise'
          }

          # Append TempRow to TopModelDetectionDF
          TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, TempRow)
        }
      }

      # Identify missed detections


      if (length( unlist(DetectionList)) > 0 &  length( unlist(DetectionList)) < nrow(TestDataTable) ) {

        missed_detections <- TestDataTable[-unlist(DetectionList), ]
        # Prepare missed detections data
        missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
        #missed_detections$Detections <- ShortName
        missed_detections$Confidence <- 0
        missed_detections$Species.Code <- detect.class
        missed_detections$Common.Name <- detect.class

        missed_detections$Class <- detect.class

        # Append missed detections to TopModelDetectionDF
        TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, missed_detections)
      }

      if (length( unlist(DetectionList)) == 0) {

        missed_detections <- TestDataTable
        # Prepare missed detections data
        missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
        missed_detections$Confidence <- 0
        #missed_detections$Detections <- ShortName
        missed_detections$Species.Code <- detect.class
        missed_detections$Common.Name <- detect.class

        missed_detections$Class <- detect.class

        # Append missed detections to TopModelDetectionDF
        TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, missed_detections)

      }

    }
  }


  TopModelDetectionDF$Class <- as.factor(TopModelDetectionDF$Class)

  # Convert Class column to a factor variable
  TopModelDetectionDF$Class <- as.factor(TopModelDetectionDF$Class)

  # Display unique values in the Class column
  unique(TopModelDetectionDF$Class)

  # Define a vector of confidence Thresholds
  Thresholds <-seq(0.1,0.9,0.1)

  # Create an empty data frame to store results
  BestF1data.framecrestedargusBinary <- data.frame()

  # Loop through each threshold value
  for(a in 1:length(Thresholds)){

    # Filter the subset based on the confidence threshold
    TopModelDetectionDF_single <-TopModelDetectionDF

    TopModelDetectionDF_single$PredictedClass <-
      ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'noise',detect.class)

    # Calculate confusion matrix using caret package
    caretConf <- caret::confusionMatrix(
      as.factor(TopModelDetectionDF_single$PredictedClass),
      as.factor(TopModelDetectionDF_single$Class),positive = detect.class,
      mode = 'everything')


    # Extract F1 score, Precision, and Recall from the confusion matrix
    F1 <- caretConf$byClass[7]
    Precision <- caretConf$byClass[5]
    Recall <- caretConf$byClass[6]
    FP <- caretConf$table[1,2]
    TN <- sum(caretConf$table[2,])
    FPR <-  FP / (FP + TN)
    # Create a row for the result and add it to the BestF1data.frameGreyGibbon
    #TrainingData <- training_data_type
    TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
    TempF1Row$Thresholds <- Thresholds[a]
    BestF1data.framecrestedargusBinary <- rbind.data.frame(BestF1data.framecrestedargusBinary, TempF1Row)
  }



  BestF1data.framecrestedargusBinary$PerformanceFolder <- basename(PerformanceFolders[[z]])

  pp <- as.numeric(TopModelDetectionDF$Confidence)
  ll <- TopModelDetectionDF$Class


  roc.s100b <- auc(roc(response=TopModelDetectionDF$Class,predictor= as.numeric(TopModelDetectionDF$Confidence)))

  BestF1data.framecrestedargusBinary$auc <- as.numeric(roc.s100b)


  CombinedF1data <- rbind.data.frame(CombinedF1data,BestF1data.framecrestedargusBinary)
}

CombinedF1data <- na.omit(CombinedF1data)
CombinedF1data$samples <- as.numeric(str_split_fixed(CombinedF1data$PerformanceFolder,pattern = 'samples',n=2)[,1])
CombinedF1data$Precision <- round(CombinedF1data$Precision,1)
CombinedF1data$Recall <- round(CombinedF1data$Recall,1)
CombinedF1data$F1 <- round(CombinedF1data$F1,1)

# levels(CombinedF1data$samples ) <- c("10 samples", "15 samples", "20 samples", "25 samples", "30 samples",
#                                      "5 samples", "All samples (LQ)", "All samples (HQ)")
#
# CombinedF1data$samples <- factor(CombinedF1data$samples, levels = c("5 samples","10 samples", "15 samples", "20 samples", "25 samples", "30 samples",
#                                                                     "All samples (LQ)", "All samples (HQ)"))

AUCPlot <- ggpubr::ggboxplot(data=CombinedF1data,x='samples',y='auc')+xlab('')+ylab('AUC')+ylim(0,1)
F1Plot <- ggpubr::ggboxplot(data=CombinedF1data,x='Thresholds',y='F1',facet.by = 'samples')+ylim(0,1)+xlab('Confidence')
ggpubr::ggboxplot(data=CombinedF1data,x='Thresholds',y='Precision',facet.by = 'samples')
PrecRec <- ggpubr::ggboxplot(data=CombinedF1data,x='Precision',y='Recall',facet.by = 'samples')

#pdf('birdNET_results.pdf',height=12,width=11)
AUCPlot
F1Plot
PrecRec
#cowplot::plot_grid(AUCPlot,F1Plot,PrecRec,nrow=3,labels = c('A)','B)','C)'),label_x = 0.9, label_y = 0.98)
#graphics.off()
ggpubr::ggboxplot(data=CombinedF1data,x='samples',y='auc')+xlab('')+ylab('AUC')+ylim(0,1)

max(CombinedF1data$F1)

