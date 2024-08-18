library(ggpubr)

devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonR")

DanumMFCCDF <- MFCCFunction(input.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/TrainingDataWavs/DanumClipsBirdNET/train/Gibbons/',
           min.freq = 400,
           max.freq = 2000,
           n.windows = 9,
           num.cep = 12,
           win.avg = 'standard',
           win.hop.time = 0.25)


DanumMFCCDFNoise <- MFCCFunction(input.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/TrainingDataWavs/DanumClipsBirdNET/train/noise/',
                            min.freq = 400,
                            max.freq = 2000,
                            n.windows = 9,
                            num.cep = 12,
                            win.avg = 'standard',
                            win.hop.time = 0.25)



DanumMFCCDFTrain <- rbind.data.frame(DanumMFCCDF,DanumMFCCDFNoise)

DanumMFCCDFTrain$signal <- as.factor(DanumMFCCDFTrain$signal )

TestFileDirectory <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/SoundFiles/'

OutputDirectory <- 'data/gibbonR/'

gibbonR(input=TestFileDirectory,
                  input.type='directory',
                  feature.df=DanumMFCCDFTrain,
                  model.type.list=c('SVM'),
                  tune = TRUE,
                  short.wav.duration=300,
                  target.signal = c("Gibbons"),
                  min.freq = 400, max.freq = 1600,
                  noise.quantile.val=0.15,
                  #time.window.number =3,
                  n.windows = 9, num.cep = 12,
                  spectrogram.window =160,
                  pattern.split = ".wav",
                  min.signal.dur = 3,
                  max.sound.event.dur = 25,
                  maximum.separation =1,
                  probability.thresh.svm = 0.15,
                  probability.thresh.rf = 0.15,
                  wav.output = "FALSE",
                  output.dir =OutputDirectory,
                  swift.time=FALSE,time.start=5,time.stop=10,
                  write.table.output=TRUE,verbose=TRUE,
                  random.sample='NA')


# Danum Binary --------------------------------------------------
# Get a list of TopModel result files
TopModelresults <- list.files(OutputDirectory,full.names = TRUE)

# Get a list of annotation selection table files
TestDataSet <- list.files('/Volumes/DJC Files/Clink et al Zenodo Data/MatchForCNN/',
                          full.names = TRUE)


# Preallocate space for TopModelDetectionDF
TopModelDetectionDF <- data.frame()

# Loop through each TopModel result file
for (a in 1:length(TopModelresults)) {

  # Read the TopModel result table into a data frame
  TempTopModelTable <- read.delim2(TopModelresults[a])

  # Extract the short name of the TopModel result file
  ShortName <- basename(TopModelresults[a])
  ShortName <- str_split_fixed(ShortName, pattern = '.wav', n = 2)[, 1]
  ShortName <- str_split_fixed(ShortName, pattern = 'gibbonR', n = 2)[, 1]

  # Find the corresponding annotation selection table
  testDataIndex <- which(str_detect(TestDataSet, ShortName))
  TestDataTable <- read.delim2(TestDataSet[testDataIndex])

  # Subset the annotation selection table to include only "LGFG" Call.Type
  TestDataTable <- subset(TestDataTable, Call.type == "female.gibbon")

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
                                         TestDataTable$Begin.Time..s. - 8,
                                         TestDataTable$Begin.Time..s. + 8)

      # Extract the detections matching the time range
      matched_detections <- TestDataTable[TimeBetween, ]

      if (nrow(matched_detections) > 0) {
        # Set signal based on the Call.Type in matched_detections
        TempRow$signal <- 'GreyGibbon'
        DetectionList[[length(DetectionList)+1]] <-  which(TimeBetween == TRUE)
      } else {
        # Set signal to 'Noise' if no corresponding annotation is found
        TempRow$signal <- 'Noise'
      }

      # Append TempRow to TopModelDetectionDF
      TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, TempRow)
    }
  }

  # Identify missed detections


  if (length(unlist(DetectionList)) > 0) {
    missed_detections <- TestDataTable[-unlist(DetectionList), ]
    # Prepare missed detections data
    missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
    missed_detections$probability <- 0
    missed_detections$File.Name <- ShortName
    missed_detections$signal <- 'GreyGibbon'
    missed_detections$model.type <- 'RF'
    # Append missed detections to TopModelDetectionDF
    TopModelDetectionDF <- rbind.data.frame(TopModelDetectionDF, missed_detections)
  }

}


WavDur <- 7200
clip_duration <- 12
hop_size <- 6

Seq.start <- list()
Seq.end <- list()

i <- 1
while (i + clip_duration < WavDur) {
  # print(i)
  Seq.start[[i]] = i
  Seq.end[[i]] = i+clip_duration
  i= i+hop_size
}


ClipStart <- unlist(Seq.start)
ClipEnd <- unlist(Seq.end)

TempClips <- cbind.data.frame(ClipStart,ClipEnd)

DanumAdj <-  nrow(TempClips)*length(TopModelresults) - nrow(TopModelDetectionDF)

# Convert signal column to a factor variable
TopModelDetectionDF$signal <- as.factor(TopModelDetectionDF$signal)

# Display unique values in the signal column
unique(TopModelDetectionDF$signal)

# Define a vector of confidence Thresholds
Thresholds <-seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameGreyGibbonBinary <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-TopModelDetectionDF

  TopModelDetectionDF_single$Predictedsignal <-  ifelse(TopModelDetectionDF_single$probability  < Thresholds[a], 'Noise','GreyGibbon')

  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$Predictedsignal),
    as.factor(TopModelDetectionDF_single$signal),positive='GreyGibbon',
    mode = 'everything')

  false_positives <- caretConf$table[1,2]
  true_negatives <- caretConf$table[2,2]

  FPR <- false_positives / (false_positives + true_negatives)

  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]

  FP <- caretConf$table[1,1]
  TN <- caretConf$table[2,2]+DanumAdj


  FPR <-  FP / (FP + TN)
  # Create a row for the result and add it to the BestF1data.frameGreyGibbonBinary
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameGreyGibbonBinary <- rbind.data.frame(BestF1data.frameGreyGibbonBinary, TempF1Row)
}


BestF1data.frameGreyGibbonBinary

GreyGibbonMax <- round(max(na.omit(BestF1data.frameGreyGibbonBinary$F1)),2)

# Metric plot
GreyGibbonBinaryPlot <- ggplot(data = BestF1data.frameGreyGibbonBinary, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Grey Gibbons (binary) \n max F1:",GreyGibbonMax),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")

GreyGibbonBinaryPlot


#write.csv(DanumMFCCDFTrain,'data/DanumMFCCDFTrain.csv',row.names = F)

tune.rad <-
  e1071::tune(
    svm,
    DanumMFCCDFTrain[, 2:ncol(DanumMFCCDFTrain)],
    DanumMFCCDFTrain$signal,
    kernel = "radial",
    tunecontrol = tune.control(cross = 5),
    ranges = list(
      cost = c(0.001, 0.01, 0.1, 1, 2,
               10, 100, 1000),
      gamma = c(0.01, 0.1, 0.5, 1, 2)
    )
  )


ml.model.svm <-
  e1071::svm(
    DanumMFCCDFTrain[, 2:ncol(DanumMFCCDFTrain)],
    DanumMFCCDFTrain$signal,
    kernel = "radial",
    gamma = tune.rad$best.parameters$gamma,
    cost = tune.rad$best.parameters$cost,
    cross = 20,
    probability = TRUE
  )

ml.model.svm$tot.accuracy


DanumTestMFCCDF <- MFCCFunction(input.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/Clips/GreyGibbons/',
                            min.freq = 400,
                            max.freq = 2000,
                            n.windows = 9,
                            num.cep = 12,
                            win.avg = 'standard',
                            win.hop.time = 0.25)


DanumMFCCDFTestNoise <- MFCCFunction(input.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/Clips/Noise/',
                                 min.freq = 400,
                                 max.freq = 2000,
                                 n.windows = 9,
                                 num.cep = 12,
                                 win.avg = 'standard',
                                 win.hop.time = 0.25)

DanumMFCCDFTest <- rbind.data.frame(DanumTestMFCCDF,DanumMFCCDFTestNoise)

DanumMFCCDFTest$signal <- as.factor(DanumMFCCDFTest$signal )

#write.csv(DanumMFCCDFTest,'data/DanumMFCCDFTest.csv',row.names = F)


# Read in pre-processed files ---------------------------------------------

#DanumMFCCDFTrain <- read.csv('data/DanumMFCCDFTrain.csv')

TestPredictions <- predict( ml.model.svm,DanumMFCCDFTest[, 2:ncol(DanumMFCCDFTest)], probability = TRUE)
TestPredictionsProb <- as.data.frame(attr(TestPredictions, "probabilities"))
TestPredictionsProb$ActualLabel <-DanumMFCCDFTest$signal

# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameGreyGibbonSVM <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-TestPredictionsProb

  TopModelDetectionDF_single$Predictedsignal <-
    ifelse(TopModelDetectionDF_single[,1]  <=Thresholds[a], 'Noise','GreyGibbons')

  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$Predictedsignal),
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
  BestF1data.frameGreyGibbonSVM <- rbind.data.frame(BestF1data.frameGreyGibbonSVM, TempF1Row)
}

BestF1data.frameGreyGibbonSVM
max(na.omit(BestF1data.frameGreyGibbonSVM$F1))
BestF1data.frameGreyGibbonSVM[which.max(na.omit(BestF1data.frameGreyGibbonSVM$F1)),]


MaxF1SVM <- round(max(na.omit(BestF1data.frameGreyGibbonSVM$F1)),2)
# Metric plot
GreyGibbonSVMPlot <- ggplot(data = BestF1data.frameGreyGibbonSVM, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Grey Gibbons (SVM + MFCC) \n",'Max F1=', MaxF1SVM),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

GreyGibbonSVMPlot


cowplot::plot_grid(GreyGibbonCNNBinary,GreyGibbonCNNMulti,GreyGibbonBirdNETPlot,GreyGibbonSVMPlot)

