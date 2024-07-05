library(ggpubr)


ClipDetections <- list.files('/Volumes/DJC Files/GunshotDataWavBelize/BirdNETOutput',
           recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Volumes/DJC Files/GunshotDataWavBelize/BirdNETOutput',
                             recursive = T,full.names = F)


BirdNETPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

 TempDF <- read.delim(ClipDetections[a])

 # Find the highest confidence for each clip
 if(nrow(TempDF) > 0){
 Confidence <- max(TempDF$Confidence)
 TempDF <- TempDF[which.max(TempDF$Confidence),]
 ActualLabel <- dirname(ClipDetectionsShort[a])

 if(TempDF$Common.Name=='nocall'){

   Confidence <- 1- Confidence
 }

 }

 TempRow <- cbind.data.frame(Confidence, ActualLabel)
 TempRow$FileName <-ClipDetectionsShort[a]
 TempRow$BirdNETBinary <- ifelse(TempRow$Confidence <= 0.5, 'Noise','Gunshot')
 BirdNETPerformanceDF <- rbind.data.frame(BirdNETPerformanceDF,TempRow)
}

table(BirdNETPerformanceDF$BirdNETBinary)
subset(BirdNETPerformanceDF,ActualLabel=='Noise'& BirdNETBinary=='Gunshot')

caretConf <- caret::confusionMatrix(
  as.factor(BirdNETPerformanceDF$BirdNETBinary),
  as.factor(BirdNETPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf

# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameGunshotBirdNET <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-BirdNETPerformanceDF

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','Gunshot')

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
  TN <- caretConf$table[2,2]#+JahooAdj
  FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)#
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameGunshotBirdNET <- rbind.data.frame(BestF1data.frameGunshotBirdNET, TempF1Row)
}

BestF1data.frameGunshotBirdNET
max(na.omit(BestF1data.frameGunshotBirdNET$F1))
BestF1data.frameGunshotBirdNET[which.max(na.omit(BestF1data.frameGunshotBirdNET$F1)),]


MaxF1BirdNET <- round(max(na.omit(BestF1data.frameGunshotBirdNET$F1)),2)
# Metric plot
GunshotBirdNETPlot <- ggplot(data = BestF1data.frameGunshotBirdNET, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Gunshot (BirdNET) \n",'Max F1=', MaxF1BirdNET),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

GunshotBirdNETPlot

