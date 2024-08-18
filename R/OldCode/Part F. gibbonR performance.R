# Load necessary libraries
library(stringr)  # For string manipulation
library(caret)    # For machine learning and model evaluation
library(ggpubr)   # For data visualization
library(dplyr)    # For data manipulation
library(data.table) # For sorting the detections
library(ggplot2)
library(ROCR)
library(pROC)
library(plyr)

# NOTE you need to change the file paths below to where your files are located on your computer
detect.signal <- 'gibbon'

#  Performance Binary --------------------------------------------------------
PerformanceFolders <- list.files('/Volumes/DJC Files/JahooGibbonModelsRandomgibbonR/',
                                 full.names = TRUE)

# Get a list of annotation selection table files
TestDataSet <-  list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/AnnotatedFiles',
                           full.names = TRUE)

start.time.buffer <- 12
end.time.buffer <- 3

CombinedF1datagibbonR <- data.frame()

for(z in 1:length(PerformanceFolders)){ tryCatch({
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
    TempTopModelTable <- TempTopModelTable [, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.","probability" ,"signal")]
    # Extract the short name of the TopModel result file
    ShortName <- basename(TopModelresults[a])
    ShortName <- str_split_fixed(ShortName, pattern = 'gibbonR', n = 2)[, 1]


    # Find the corresponding annotation selection table
    testDataIndex <- which(str_detect(TestDataSet, ShortName))

    if(length(testDataIndex) > 0){
      TestDataTable <- read.delim2(TestDataSet[testDataIndex])

      TestDataTable$signal <- detect.signal
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
            # Set signal based on the Call.Type in matched_detections
            TempRow$signal <- detect.signal
            DetectionList[[length( unlist(DetectionList))+1]] <-  which(TimeBetween == TRUE)
          } else {
            # Set signal to 'noise' if no corresponding annotation is found
            TempRow$signal <- 'noise'
          }

          TempRow$Detections <-  ShortName
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
        missed_detections$probability <- 0

        missed_detections$signal <- detect.signal

        missed_detections$Detections <-  ShortName
        # Append missed detections to TopModelDetectionDF
        TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, missed_detections)
      }

      if (length( unlist(DetectionList)) == 0) {

        missed_detections <- TestDataTable
        # Prepare missed detections data
        missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
        missed_detections$probability <- 0

        missed_detections$signal <- detect.signal
        missed_detections$Detections <-  ShortName
        # Append missed detections to TopModelDetectionDF
        TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, missed_detections)

      }

    }
  }


  # Convert signal column to a factor variable
  TopModelDetectionDF$signal <- as.factor(TopModelDetectionDF$signal)

  # Display unique values in the signal column
  unique(TopModelDetectionDF$signal)

  TopModelDetectionDF$probability <- as.numeric(  TopModelDetectionDF$probability)
  # Define a vector of probability Thresholds
  Thresholds <-seq(0.1,0.9,0.1)

  # Create an empty data frame to store results
  BestF1data.framecrestedargusBinary <- data.frame()

  # Loop through each threshold value
  for(a in 1:length(Thresholds)){   tryCatch({

    # Filter the subset based on the probability threshold
    TopModelDetectionDF_single <-TopModelDetectionDF

    TopModelDetectionDF_single$Predictedsignal <-
      ifelse(TopModelDetectionDF_single$probability  <=Thresholds[a], 'noise',detect.signal)

    # Calculate confusion matrix using caret package
    caretConf <- caret::confusionMatrix(
      as.factor(TopModelDetectionDF_single$Predictedsignal),
      as.factor(TopModelDetectionDF_single$signal),positive = detect.signal,
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

  }, error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  })
  }


  BestF1data.framecrestedargusBinary$PerformanceFolder <- basename(PerformanceFolders[[z]])

  pp <- as.numeric(TopModelDetectionDF$probability)
  ll <- TopModelDetectionDF$signal


  roc.s100b <- auc(roc(response=TopModelDetectionDF$signal,predictor= as.numeric(TopModelDetectionDF$probability)))

  BestF1data.framecrestedargusBinary$auc <- as.numeric(roc.s100b)

  CombinedF1datagibbonR <- rbind.data.frame(CombinedF1datagibbonR,BestF1data.framecrestedargusBinary)
}, error = function(e) {
  cat("ERROR :", conditionMessage(e), "\n")
})
}

CombinedF1datagibbonR <- na.omit(CombinedF1datagibbonR)
CombinedF1datagibbonR$samples <- str_split_fixed(CombinedF1datagibbonR$PerformanceFolder,pattern = 'samples',n=2)[,1]
CombinedF1datagibbonR$samples <- as.numeric(str_split_fixed(CombinedF1datagibbonR$samples ,pattern = '_',n=2)[,2])
CombinedF1datagibbonR$Precision <- round(CombinedF1datagibbonR$Precision,1)
CombinedF1datagibbonR$Recall <- round(CombinedF1datagibbonR$Recall,2)
CombinedF1datagibbonR$F1 <- round(CombinedF1datagibbonR$F1,2)

# levels(CombinedF1datagibbonR$samples ) <- c("10 samples", "15 samples", "20 samples", "25 samples", "30 samples",
#                                      "5 samples", "All samples (LQ)", "All samples (HQ)")
#
# CombinedF1datagibbonR$samples <- factor(CombinedF1datagibbonR$samples, levels = c("5 samples","10 samples", "15 samples", "20 samples", "25 samples", "30 samples",
#                                                                     "All samples (LQ)", "All samples (HQ)"))

AUCPlot <- ggpubr::ggerrorplot(data=CombinedF1datagibbonR,x='samples',y='auc')+xlab('')+ylab('AUC')+ylim(0,1)
F1Plot <- ggpubr::ggerrorplot(data=CombinedF1datagibbonR,x='Thresholds',y='F1',facet.by = 'samples')+ylim(0,1)+xlab('probability')
ggpubr::ggerrorplot(data=CombinedF1datagibbonR,x='Thresholds',y='Precision',facet.by = 'samples')
PrecRec <- ggpubr::ggerrorplot(data=CombinedF1datagibbonR,x='Precision',y='Recall',facet.by = 'samples')

#pdf('birdNET_results.pdf',height=12,width=11)
AUCPlot
F1Plot + geom_hline(yintercept = 0.8, color='red',linetype='dashed')
PrecRec

ggpubr::ggerrorplot(data=CombinedF1datagibbonR,x='Thresholds',y='F1',facet.by = 'samples')+ylim(0,1)+xlab('probability')

which.max(CombinedF1datagibbonR$F1)

CombinedF1datagibbonR[which.max(CombinedF1datagibbonR$F1),]

MaxF1PlotCNN <- CombinedF1datagibbonR %>%
  dplyr::group_by(samples) %>%
  dplyr::summarise(F1 = max(F1, na.rm=TRUE))

CNN <- ggpubr::ggline(data=CombinedF1datagibbonR,x='samples',y='F1',add = "mean_se")+ggtitle('CNN')+ylim(0,1)+xlab('')

CombinedPlot <- cowplot::plot_grid(BirdNET,CNN)+xlab('Number of training samples')

ggdraw(add_sub(CombinedPlot, "Number of training samples", vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))
