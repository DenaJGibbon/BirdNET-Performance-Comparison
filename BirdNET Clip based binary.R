library(ggpubr)

ClipDetections <- list.files('/Volumes/DJC Files/BirdNETBinaryClipBasedVietnam',
                             recursive = T,full.names = T)

ClipDetectionsShort <-  dirname(ClipDetections)

BirdNETBinaryPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

  TempDF <- read.delim(ClipDetections[a])

  TempDF <-  subset(TempDF,Common.Name=='CrestedGibbons' | Common.Name=='gibbon'|Common.Name=='Gibbons')
  # Find the highest confidence for each clip
  if(nrow(TempDF) > 0){
    Confidence <- max(TempDF$Confidence)
    TempDF <- TempDF[which.max(TempDF$Confidence),]
    ActualLabel <- basename(ClipDetectionsShort[a])
  } else{
    # TempDF <- read.delim(ClipDetections[a])
    # Confidence <- 1-max(TempDF$Confidence)
  Confidence <- 0
    ActualLabel <- basename(ClipDetectionsShort[a])
  }

  TempRow <- cbind.data.frame(Confidence, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$BirdNETBinaryBinary <- ifelse(TempRow$Confidence <= 0.5, 'Noise','Gibbon')

  TempRow$samples <- str_split_fixed(ClipDetectionsShort[a],'samples',n=2)[,1]
  TempRow$samples <-basename(  TempRow$samples )
  TempRow$PerformanceFolder <-  str_split_fixed(ClipDetectionsShort[a],pattern = '/',n=6)[,5]

  BirdNETBinaryPerformanceDF <- rbind.data.frame(BirdNETBinaryPerformanceDF,TempRow)
}

#subset(BirdNETBinaryPerformanceDF,ActualLabel=='Noise'& BirdNETBinaryBinary=='CrestedGibbons')

caretConf <- caret::confusionMatrix(
  as.factor(BirdNETBinaryPerformanceDF$BirdNETBinaryBinary),
  as.factor(BirdNETBinaryPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf


uniquesamples <- unique(BirdNETBinaryPerformanceDF$PerformanceFolder)

SampleSizeCrestedGibbonBirdNETBinary <- data.frame()

for(z in 1:length(uniquesamples)){
# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.1)

# Create an empty data frame to store results

BirdNETBinaryPerformanceDFtemp <- subset(BirdNETBinaryPerformanceDF,PerformanceFolder==uniquesamples[z])

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-BirdNETBinaryPerformanceDFtemp

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','Gibbon')

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
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the SampleSizeGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  TempF1Row$samples <- uniquesamples[z]
  TempF1Row$performancefolder <- uniquesamples[z]
  SampleSizeCrestedGibbonBirdNETBinary <- rbind.data.frame(SampleSizeCrestedGibbonBirdNETBinary, TempF1Row)
}
}

SampleSizeCrestedGibbonBirdNETBinary <- na.omit(SampleSizeCrestedGibbonBirdNETBinary)
SampleSizeCrestedGibbonBirdNETBinary$Precision <- round(SampleSizeCrestedGibbonBirdNETBinary$Precision,1)
SampleSizeCrestedGibbonBirdNETBinary$Recall <- round(SampleSizeCrestedGibbonBirdNETBinary$Recall,2)
SampleSizeCrestedGibbonBirdNETBinary$F1 <- as.numeric(round(SampleSizeCrestedGibbonBirdNETBinary$F1,2))

MaxF1PlotBirdNETbinary <- SampleSizeCrestedGibbonBirdNETBinary %>%
  dplyr::group_by(performancefolder) %>%
  dplyr::summarise(F1 = max(F1, na.rm=TRUE))

MaxF1PlotBirdNETbinary$samples <-  as.numeric(str_split_fixed(MaxF1PlotBirdNETbinary$performancefolder,pattern = 'samples',
                n=2)[,1])

MaxF1PlotBirdNETbinary$F1 <- as.numeric(round(MaxF1PlotBirdNETbinary$F1,2))

ggpubr::ggline(data=MaxF1PlotBirdNETbinary,x='samples',y='F1',add = "mean_se")+ylim(0,1)+ggtitle('BirdNET binary')




# Multi -------------------------------------------------------------------

library(ggpubr)

ClipDetections <- list.files('/Volumes/DJC Files/BirdNETMultiClipBasedVietnam',
                             recursive = T,full.names = T)

ClipDetectionsShort <-  dirname(ClipDetections)

BirdNETmultiPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

  TempDF <- read.delim(ClipDetections[a])


  TempDF <-  subset(TempDF,Common.Name=='CrestedGibbon' |Common.Name=='CrestedGibbons' | Common.Name=='gibbon'|Common.Name=='Gibbons')
  # Find the highest confidence for each clip
  if(nrow(TempDF) > 0){
    Confidence <- max(TempDF$Confidence)
    TempDF <- TempDF[which.max(TempDF$Confidence),]
    ActualLabel <- basename(ClipDetectionsShort[a])
  } else{
    TempDF <- read.delim(ClipDetections[a])

    Confidence <- 0

    ActualLabel <- basename(ClipDetectionsShort[a])
  }

  TempRow <- cbind.data.frame(Confidence, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$BirdNETmultimulti <- ifelse(TempRow$Confidence <= 0.5, 'Noise','Gibbon')

  TempRow$samples <- str_split_fixed(ClipDetectionsShort[a],'samples',n=2)[,1]
  TempRow$samples <-basename(  TempRow$samples )
  TempRow$PerformanceFolder <-  str_split_fixed(ClipDetectionsShort[a],pattern = '/',n=6)[,5]

  BirdNETmultiPerformanceDF <- rbind.data.frame(BirdNETmultiPerformanceDF,TempRow)
}

#subset(BirdNETmultiPerformanceDF,ActualLabel=='Noise'& BirdNETmultimulti=='CrestedGibbons')

caretConf <- caret::confusionMatrix(
  as.factor(BirdNETmultiPerformanceDF$BirdNETmultimulti),
  as.factor(BirdNETmultiPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf


uniquesamples <- unique(BirdNETmultiPerformanceDF$PerformanceFolder)

SampleSizeCrestedGibbonBirdNETmulti <- data.frame()

for(z in 1:length(uniquesamples)){
  # Define a vector of confidence Thresholds
  Thresholds <- seq(0.1,1,0.1)

  # Create an empty data frame to store results

  BirdNETmultiPerformanceDFtemp <- subset(BirdNETmultiPerformanceDF,PerformanceFolder==uniquesamples[z])

  # Loop through each threshold value
  for(a in 1:length(Thresholds)){

    # Filter the subset based on the confidence threshold
    TopModelDetectionDF_single <-BirdNETmultiPerformanceDFtemp

    TopModelDetectionDF_single$PredictedClass <-
      ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','Gibbon')

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
    # TN <- caretConf$table[2,2]+JahooAdj
    # FPR <-  FP / (FP + TN)
    # # Create a row for the result and add it to the SampleSizeGreyGibbon
    #TrainingData <- training_data_type
    TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
    TempF1Row$Thresholds <- Thresholds[a]
    TempF1Row$samples <- uniquesamples[z]
    TempF1Row$performancefolder <- uniquesamples[z]
    SampleSizeCrestedGibbonBirdNETmulti <- rbind.data.frame(SampleSizeCrestedGibbonBirdNETmulti, TempF1Row)
  }
}

SampleSizeCrestedGibbonBirdNETmulti <- na.omit(SampleSizeCrestedGibbonBirdNETmulti)
SampleSizeCrestedGibbonBirdNETmulti$Precision <- round(SampleSizeCrestedGibbonBirdNETmulti$Precision,1)
SampleSizeCrestedGibbonBirdNETmulti$Recall <- round(SampleSizeCrestedGibbonBirdNETmulti$Recall,2)
SampleSizeCrestedGibbonBirdNETmulti$F1 <- as.numeric(round(SampleSizeCrestedGibbonBirdNETmulti$F1,2))

MaxF1PlotBirdNETmulti <- SampleSizeCrestedGibbonBirdNETmulti %>%
  dplyr::group_by(performancefolder) %>%
  dplyr::summarise(F1 = max(F1, na.rm=TRUE))

MaxF1PlotBirdNETmulti$samples <-  as.numeric(str_split_fixed(MaxF1PlotBirdNETmulti$performancefolder,pattern = 'samples',
                                                              n=2)[,1])

MaxF1PlotBirdNETmulti$F1 <- as.numeric(round(MaxF1PlotBirdNETmulti$F1,2))

ggpubr::ggline(data=MaxF1PlotBirdNETmulti,x='samples',y='F1',add = "mean_se")+ylim(0,1)+ggtitle('BirdNET multi')

