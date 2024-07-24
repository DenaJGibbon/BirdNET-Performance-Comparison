library(ggpubr)

ClipDetections <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonAddDanum',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonAddDanum/',
                                  recursive = T,full.names = F)

BirdNETMultiPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

  TempDF <- read.delim(ClipDetections[a])

  TempDF <-  subset(TempDF,Common.Name=='CrestedGibbons')
  # Find the highest confidence for each clip
  if(nrow(TempDF) > 0){
    Confidence <- max(TempDF$Confidence)
    TempDF <- TempDF[which.max(TempDF$Confidence),]
    ActualLabel <- dirname(ClipDetectionsShort[a])
  } else{

    Confidence <- 0

    ActualLabel <- dirname(ClipDetectionsShort[a])
  }

  TempRow <- cbind.data.frame(Confidence, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$BirdNETMultiBinary <- ifelse(TempRow$Confidence <= 0.5, 'Noise','CrestedGibbons')
  BirdNETMultiPerformanceDF <- rbind.data.frame(BirdNETMultiPerformanceDF,TempRow)
}

#subset(BirdNETMultiPerformanceDF,ActualLabel=='Noise'& BirdNETMultiBinary=='CrestedGibbons')

caretConf <- caret::confusionMatrix(
  as.factor(BirdNETMultiPerformanceDF$BirdNETMultiBinary),
  as.factor(BirdNETMultiPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf

# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.005)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonBirdNETMulti <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-BirdNETMultiPerformanceDF

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
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonBirdNETMulti <- rbind.data.frame(BestF1data.frameCrestedGibbonBirdNETMulti, TempF1Row)
}

BestF1data.frameCrestedGibbonBirdNETMulti
max(na.omit(BestF1data.frameCrestedGibbonBirdNETMulti$F1))
BestF1data.frameCrestedGibbonBirdNETMulti[which.max(na.omit(BestF1data.frameCrestedGibbonBirdNETMulti$F1)),]


# performancetables.dir.multi.balanced.true <-'/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_allmodels/performance_tables_trained'
#
# PerformanceOutputmulti.balanced.true <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir.multi.balanced.true,
#                                                                          class='CrestedGibbons',
#                                                                          model.type = "binary",
#                                                                          Thresh.val = 0.1)
#
# PerformanceOutputmulti.balanced.true$f1_plot
# PerformanceOutputmulti.balanced.true$best_f1$F1



MaxF1BirdNETMulti <- round(max(na.omit(BestF1data.frameCrestedGibbonBirdNETMulti$F1)),2)
# Metric plot
CrestedGibbonBirdNETMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNETMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (BirdNET Multi) \n",'Max F1=', MaxF1BirdNETMulti),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonBirdNETMultiPlot


ClipDetections <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonIgnoreWindows/',
           recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonIgnoreWindows/',
                             recursive = T,full.names = F)


BirdNETPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

 TempDF <- read.delim(ClipDetections[a])

 TempDF <-  subset(TempDF,Common.Name=='Gibbons')
 # Find the highest confidence for each clip
 if(nrow(TempDF) > 0){
 Confidence <- max(TempDF$Confidence)
 TempDF <- TempDF[which.max(TempDF$Confidence),]
 ActualLabel <- dirname(ClipDetectionsShort[a])
 } else{

   Confidence <- 0

   ActualLabel <- dirname(ClipDetectionsShort[a])
 }

 TempRow <- cbind.data.frame(Confidence, ActualLabel)
 TempRow$FileName <-ClipDetectionsShort[a]
 TempRow$BirdNETBinary <- ifelse(TempRow$Confidence <= 0.5, 'Noise','CrestedGibbons')
 BirdNETPerformanceDF <- rbind.data.frame(BirdNETPerformanceDF,TempRow)
}

#subset(BirdNETPerformanceDF,ActualLabel=='Noise'& BirdNETBinary=='CrestedGibbons')

caretConf <- caret::confusionMatrix(
  as.factor(BirdNETPerformanceDF$BirdNETBinary),
  as.factor(BirdNETPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf

# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.005)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonBirdNET <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-BirdNETPerformanceDF

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
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonBirdNET <- rbind.data.frame(BestF1data.frameCrestedGibbonBirdNET, TempF1Row)
}

BestF1data.frameCrestedGibbonBirdNET
max(na.omit(BestF1data.frameCrestedGibbonBirdNET$F1))
BestF1data.frameCrestedGibbonBirdNET[which.max(na.omit(BestF1data.frameCrestedGibbonBirdNET$F1)),]


# performancetables.dir.multi.balanced.true <-'/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_allmodels/performance_tables_trained'
#
# PerformanceOutputmulti.balanced.true <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir.multi.balanced.true,
#                                                                          class='CrestedGibbons',
#                                                                          model.type = "binary",
#                                                                          Thresh.val = 0.1)
#
# PerformanceOutputmulti.balanced.true$f1_plot
# PerformanceOutputmulti.balanced.true$best_f1$F1



MaxF1BirdNET <- round(max(na.omit(BestF1data.frameCrestedGibbonBirdNET$F1)),2)
# Metric plot
CrestedGibbonBirdNETPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNET, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (BirdNET Binary) \n",'Max F1=', MaxF1BirdNET),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonBirdNETPlot

CrestedTopBinary <- read.csv('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_all/performance_tables_trained/imagescambodia_3_resnet50_model_TransferLearningTrainedModel.csv')
MaxF1Binary <- round(max(na.omit(CrestedTopBinary$F1)),2)

CrestedGibbonCNNBinary <- ggplot(data = CrestedTopBinary, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (ResNet50 Binary) \n",'Max F1=', MaxF1Binary),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)


CrestedTopMulti <- read.csv('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_multi_ignore/performance_tables_multi_trained/imagesmulti_5_resnet50_model_TransferLearningTrainedModel.csv')
MaxF1Multi <- round(max(na.omit(CrestedTopMulti$F1)),2)

CrestedGibbonCNNMulti <- ggplot(data = CrestedTopMulti, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (ResNet50 Multi) \n",'Max F1=', MaxF1Multi),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)


cowplot::plot_grid(CrestedGibbonCNNBinary,CrestedGibbonCNNMulti,CrestedGibbonBirdNETPlot)


BestF1data.frameCrestedGibbonSVM <- read.csv('data/BestF1data.frameCrestedGibbonSVM.csv')
MaxF1SVM <- round(max( na.omit(BestF1data.frameCrestedGibbonSVM$F1)),2)

# Metric plot
CrestedGibbonSVMPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVM, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons Binary (SVM + MFCC) \n",'Max F1=', MaxF1SVM),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonSVMPlot


BestF1data.frameCrestedGibbonSVMMulti <- read.csv('data/BestF1data.frameCrestedGibbonSVMMulti.csv')
MaxF1SVMMulti <- round(max( na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1)),2)

# Metric plot
CrestedGibbonSVMMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVMMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons Multi (SVM + MFCC) \n",'Max F1=', MaxF1SVMMulti),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonSVMMultiPlot

cowplot::plot_grid(CrestedGibbonSVMPlot, CrestedGibbonSVMMultiPlot,
                   CrestedGibbonKooguBinaryPlot,CrestedGibbonKooguMultiPlot,
                   CrestedGibbonCNNBinary,CrestedGibbonCNNMulti,
                   CrestedGibbonBirdNETPlot,CrestedGibbonBirdNETMultiPlot,
                   nrow=4, labels=c('A)','B)','C)','D)','E)','F)','G)','H)'),
                   label_x = 0.9)

