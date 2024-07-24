library(plyr)
library(ggpubr)
ClipDetections <- list.files('/Users/denaclink/Desktop/VSCodeRepos/BEANS/detections',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Users/denaclink/Desktop/VSCodeRepos/BEANS/detections/',
                                  recursive = T,full.names = F)

KooguBinaryPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

  TempDF <- read.delim(ClipDetections[a])

  TempDF <-  subset(TempDF,Tags=='Gibbons')
  # Find the highest Score for each clip
  if(nrow(TempDF) > 0){
    Score <- max(TempDF$Score)
    TempDF <- TempDF[which.max(TempDF$Score),]
    ActualLabel <-dirname(ClipDetectionsShort[a])
  } else{

    Score <- 0

    ActualLabel <- dirname(ClipDetectionsShort[a])
  }

  TempRow <- cbind.data.frame(Score, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$KooguBinaryBinary <- ifelse(TempRow$Score <= 0.5, 'Noise','Gibbons')
  KooguBinaryPerformanceDF <- rbind.data.frame(KooguBinaryPerformanceDF,TempRow)
}

head(KooguBinaryPerformanceDF)

KooguBinaryPerformanceDF$ActualLabel <- revalue(KooguBinaryPerformanceDF$ActualLabel, c(CrestedGibbons = "Gibbons"))

caretConf <- caret::confusionMatrix(
  as.factor(KooguBinaryPerformanceDF$KooguBinaryBinary),
  as.factor(KooguBinaryPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf

# Define a vector of Score Thresholds
Thresholds <- seq(0.1,1,0.005)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonKooguBinary <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the Score threshold
  TopModelDetectionDF_single <-KooguBinaryPerformanceDF

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Score  <=Thresholds[a], 'Noise','Gibbons')

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
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonKooguBinary <- rbind.data.frame(BestF1data.frameCrestedGibbonKooguBinary, TempF1Row)
}

BestF1data.frameCrestedGibbonKooguBinary
max(na.omit(BestF1data.frameCrestedGibbonKooguBinary$F1))
BestF1data.frameCrestedGibbonKooguBinary[which.max(na.omit(BestF1data.frameCrestedGibbonKooguBinary$F1)),]



MaxF1KooguBinary <- round(max(na.omit(BestF1data.frameCrestedGibbonKooguBinary$F1)),2)
# Metric plot
CrestedGibbonKooguBinaryPlot <- ggplot(data = BestF1data.frameCrestedGibbonKooguBinary, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (Koogu Binary) \n",'Max F1=', MaxF1KooguBinary),
       x = "Score",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonKooguBinaryPlot

# Koogu gibbon multi ----------------------------------------------------


ClipDetections <- list.files('/Users/denaclink/Desktop/VSCodeRepos/BEANS/detections_multi',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Users/denaclink/Desktop/VSCodeRepos/BEANS/detections_multi/',
                                  recursive = T,full.names = F)

KooguMultiPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

  TempDF <- read.delim(ClipDetections[a])

  TempDF <-  subset(TempDF,Tags=='CrestedGibbons')
  # Find the highest Score for each clip
  if(nrow(TempDF) > 0){
    Score <- max(TempDF$Score)
    TempDF <- TempDF[which.max(TempDF$Score),]
    ActualLabel <-dirname(ClipDetectionsShort[a])
  } else{

    Score <- 0

    ActualLabel <- dirname(ClipDetectionsShort[a])
  }

  TempRow <- cbind.data.frame(Score, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$KooguMultiMulti <- ifelse(TempRow$Score <= 0.5, 'Noise','CrestedGibbons')
  KooguMultiPerformanceDF <- rbind.data.frame(KooguMultiPerformanceDF,TempRow)
}

tail(KooguMultiPerformanceDF)


caretConf <- caret::confusionMatrix(
  as.factor(KooguMultiPerformanceDF$KooguMultiMulti),
  as.factor(KooguMultiPerformanceDF$ActualLabel),
  mode = 'everything')

caretConf

# Define a vector of Score Thresholds
Thresholds <- seq(0.1,1,0.005)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonKooguMulti <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the Score threshold
  TopModelDetectionDF_single <-KooguMultiPerformanceDF

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Score  <=Thresholds[a], 'Noise','CrestedGibbons')

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
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonKooguMulti <- rbind.data.frame(BestF1data.frameCrestedGibbonKooguMulti, TempF1Row)
}

BestF1data.frameCrestedGibbonKooguMulti
max(na.omit(BestF1data.frameCrestedGibbonKooguMulti$F1))
BestF1data.frameCrestedGibbonKooguMulti[which.max(na.omit(BestF1data.frameCrestedGibbonKooguMulti$F1)),]



MaxF1KooguMulti <- round(max(na.omit(BestF1data.frameCrestedGibbonKooguMulti$F1)),2)
# Metric plot
CrestedGibbonKooguMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonKooguMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (Koogu Multi) \n",'Max F1=', MaxF1KooguMulti),
       x = "Score",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonKooguMultiPlot


