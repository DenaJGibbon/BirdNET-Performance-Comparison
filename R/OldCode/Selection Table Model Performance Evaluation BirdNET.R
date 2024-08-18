# Load necessary libraries
library(stringr)  # For string manipulation
library(caret)    # For machine learning and model evaluation
library(ggpubr)   # For data visualization
library(dplyr)    # For data manipulation
library(data.table) # For sorting the detections
library(ggplot2)

# NOTE you need to change the file paths below to where your files are located on your computer

# KSWS Performance Binary --------------------------------------------------------

# Get a list of TopModel result files
TopModelresults <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonBandPass/',
                              full.names = TRUE)

# Get a list of annotation selection table files
TestDataSet <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/AnnotatedFiles',
                          full.names = TRUE)

# Create an empty data frame to store TopModel detection results
TopModelDetectionDF <- data.frame()

# Loop through each TopModel result file
for (a in 1:length(TopModelresults)) {
  
    # Read the TopModel result table into a data frame
    TempTopModelTable <- read.delim2(TopModelresults[a])
    TempTopModelTable$Begin.Time..s. <- as.numeric(TempTopModelTable$Begin.Time..s.)
    
    SumList <- list()
    for(b in 1: (nrow(TempTopModelTable) -1) ){
      print(b)
      SumList[[b]] <-   as.numeric(TempTopModelTable[b+1,]$Begin.Time..s.) - as.numeric(TempTopModelTable[b,]$End.Time..s.)

    }

    consecutive_groups <- split(TempTopModelTable, cumsum(c(TRUE, as.numeric(TempTopModelTable[2:nrow(TempTopModelTable),]$Begin.Time..s.) - as.numeric(TempTopModelTable[1:(nrow(TempTopModelTable) - 1),]$End.Time..s.) > 3)))

    # Display the consecutive groups
    print(consecutive_groups)

    TempTopModelTableComb <- data.frame()
    for(y in 1:length(consecutive_groups)){

     TempRow <-  consecutive_groups[[y]]

     Confidence <- max(TempRow$Confidence)
     MaxEnd <- max(TempRow$End.Time..s.)
     NewTempDetection <- TempRow[1,]

     NewTempDetection$Confidence <- Confidence
     NewTempDetection$End.Time..s. <- MaxEnd
     TempTopModelTableComb <- rbind.data.frame(TempTopModelTableComb,NewTempDetection)
    }


    #TempTopModelTableComb <- TempTopModelTable
    TempTopModelTableComb <-  TempTopModelTableComb[,c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", 
                                                       "Low.Freq..Hz.", "High.Freq..Hz.", "Confidence", "Species.Code", 
                                                       "Common.Name")]
    
     # Check if the data frame has rows
    if (nrow(TempTopModelTableComb) > 0) {
      
      # Extract the short name of the TopModel result file
      ShortName <- basename(TopModelresults[a])
      ShortName <- str_split_fixed(ShortName, pattern = '.BirdNET', n = 2)[, 1]
      #ShortName <- str_split_fixed(ShortName, pattern = '-__', n = 2)[, 2]
      
      # Find the corresponding annotation selection table
      testDataIndex <- which(str_detect(TestDataSet, ShortName))
      TestDataTable <- read.delim2(TestDataSet[testDataIndex])
      
      # Subset the annotation selection table to include only "LGFG" Call.Type
      #TestDataTable <- subset(TestDataTable, Call.Type == "LGFG")
      
      # Round Begin.Time..s. and End.Time..s. columns to numeric
      TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
      TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))
      
      TopModelDetectionDFtemp <- data.frame()
      DetectionList <- list()
      # Loop through each row in TempTopModelTableComb
      for (c in 1:nrow(TempTopModelTableComb)) {
        TempRow <- TempTopModelTableComb[c,]
        
        # Check if Begin.Time..s. is not NA
        if (is.na(TempRow$Begin.Time..s.) == FALSE) {
          # Convert Begin.Time..s. and End.Time..s. to numeric
          TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
          TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)
          
          # Determine if the time of the detection is within the time range of an annotation
          TimeBetween <- data.table::between(TempRow$Begin.Time..s.,
                                             TestDataTable$Begin.Time..s.-12,
                                             TestDataTable$Begin.Time..s.+12)
          
          # TimeBetweenEnd <- data.table::between(TempRow$End.Time..s.,
          #                                    TestDataTable$End.Time..s.-3,
          #                                    TestDataTable$End.Time..s.+3)
          #  
          # Extract the detection for which TimeBetween and TimeBetweenEnd are TRUE
         
          # For now focus only on start time
           
          TempDetection <- TestDataTable[  which(TimeBetween == TRUE), ] # | TimeBetweenEnd == TRUE
          
          if (nrow(TempDetection) > 0) {
            # Set Class based on the Call.Type in TempDetection
            TempRow$Class <- TempDetection$Class[1]
            DetectionList[[length(DetectionList)+1]] <-  which(TimeBetween == TRUE)
          } else {
            # Set Class to 'Noise' if no corresponding annotation is found
            TempRow$Class <- 'Noise'
            # Uncomment the line below if you want to modify the Confidence value
            # TempRow$Confidence <- 1 - as.numeric(TempRow$Confidence)
          }
          
          # Set TrainingData to the basename of TopModel results directory
          #TempRow$TrainingData <- basename(TopModelresults[a])
          
   
          # Append TempRow to TopModelDetectionDF
          #TopModelDetectionDFtemp <- rbind.data.frame(TopModelDetectionDFtemp, TempRow)
          TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, TempRow)
        }
      }
    } 
    
    
    IndexMissed <-  TestDataTable[ -unlist(DetectionList),]

  if( length(IndexMissed) >0 ){
    print(IndexMissed)
    IndexMissed <-   IndexMissed[,c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", 
      "Low.Freq..Hz.", "High.Freq..Hz.")]
  
    IndexMissed$Confidence <- 0
    IndexMissed$Species.Code <- 'CrestedGibbons'
    IndexMissed$Common.Name <- 'CrestedGibbons'
    IndexMissed$Class <- 'CrestedGibbons'
    
    TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, IndexMissed)
    
    }  
  }

head(TopModelDetectionDF)
nrow(TopModelDetectionDF)
table(TopModelDetectionDF$Class)

TotalSecondsDetectionsJahoo <- sum(TopModelDetectionDF$End.Time..s.-TopModelDetectionDF$Begin.Time..s.)

WavDur <- 60*60
clip_duration <- 3
hop_size <- 0

JahooAdj <-  WavDur*length(TopModelresults) - TotalSecondsDetectionsJahoo

# Convert Class column to a factor variable
TopModelDetectionDF$Class <- as.factor(TopModelDetectionDF$Class)

# Display unique values in the Class column
unique(TopModelDetectionDF$Class)

# Define a vector of confidence Thresholds
Thresholds <- seq(0,1,0.05)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonBirdNET <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-TopModelDetectionDF
  
  TopModelDetectionDF_single$PredictedClass <-  
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','CrestedGibbons')
  
    # Calculate confusion matrix using caret package
    caretConf <- caret::confusionMatrix(
      as.factor(TopModelDetectionDF_single$PredictedClass),
      as.factor(TopModelDetectionDF_single$Class),
      mode = 'everything')
    

    # Extract F1 score, Precision, and Recall from the confusion matrix
    F1 <- caretConf$byClass[7]
    Precision <- caretConf$byClass[5]
    Recall <- caretConf$byClass[6]
    FP <- caretConf$table[1,2]
    TN <- caretConf$table[2,2]+JahooAdj
    FPR <-  FP / (FP + TN)
    # Create a row for the result and add it to the BestF1data.frameGreyGibbon
    #TrainingData <- training_data_type
    TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
    TempF1Row$Thresholds <- Thresholds[a]
    BestF1data.frameCrestedGibbonBirdNET <- rbind.data.frame(BestF1data.frameCrestedGibbonBirdNET, TempF1Row)
  }

BestF1data.frameCrestedGibbonBirdNET
max(na.omit(BestF1data.frameCrestedGibbonBirdNET$F1))

# Metric plot
CrestedGibbonBirdNETPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNET, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = "Crested Gibbons (BirdNET)",
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonBirdNETPlot



# By Clip Performance -----------------------------------------------------

TempFiles <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonClips/',recursive = T, full.names = T)

CombinedBirdNETDF <- data.frame()
for(a in 1:length(TempFiles)){
 TempTable<-  read.delim(TempFiles[a])
 TempDir <- str_split_fixed(TempFiles[a], pattern = 'Clips//',n=2)[,2]
 ActualLabel <- str_split_fixed(TempDir, pattern = '/',n=2)[,1]
 TempTable$ActualLabel <- ActualLabel

 TempTable$Confidence[ which(TempTable$Species.Code=='nocall')] <- 1-  TempTable$Confidence[ which(TempTable$Species.Code=='nocall')]
  
 if(ActualLabel=="CrestedGibbons"){
   
   TempTable <-  TempTable[which.max(TempTable$Confidence),]
   
 }
 
 CombinedBirdNETDF <- rbind.data.frame(CombinedBirdNETDF,TempTable)
}


# Create an empty data frame to store results
BestF1data.frameCrestedGibbonBirdNETClips <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-CombinedBirdNETDF
  
  TopModelDetectionDF_single$PredictedClass <-  
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','CrestedGibbons')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  TN <- caretConf$table[2,2]+JahooAdj
  FPR <-  FP / (FP + TN)
  # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonBirdNETClips <- rbind.data.frame(BestF1data.frameCrestedGibbonBirdNETClips, TempF1Row)
}

BestF1data.frameCrestedGibbonBirdNETClips

# Metric plot
CrestedGibbonBirdNETClipsPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNETClips, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = "Crested Gibbons (BirdNETClips)",
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonBirdNETClipsPlot


# cowplot::plot_grid(CrestedGibbonBinaryPlot,
#                    CrestedGibbonMultiPlot,CrestedGibbonBirdNETPlot,
#                    CrestedGibbonBirdNETClipsPlot,
#                    labels=c('A)','B)','C)', 'D)'), label_x = 0.9)



