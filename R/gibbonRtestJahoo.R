devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonR")
library(ggplot2)

JahooMFCCDF <- MFCCFunction(input.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/TrainingDataWavs/JahooClipsBirdNET/train/Gibbons/',
                            min.freq = 400,
                            max.freq = 3000,
                            n.windows = 9,
                            num.cep = 12,
                            win.avg = 'standard',
                            win.hop.time = 0.25)


JahooMFCCDFNoise <- MFCCFunction(input.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/TrainingDataWavs/JahooClipsBirdNET/train/noise/',
                                 min.freq = 400,
                                 max.freq = 3000,
                                 n.windows = 9,
                                 num.cep = 12,
                                 win.avg = 'standard',
                                 win.hop.time = 0.25)



JahooMFCCDFTrain <- rbind.data.frame(JahooMFCCDF,JahooMFCCDFNoise)

JahooMFCCDFTrain$class <- as.factor(JahooMFCCDFTrain$class )


tune.rad <-
  e1071::tune(
    svm,
    JahooMFCCDFTrain[,2: (ncol(JahooMFCCDFTrain) -1)],
    JahooMFCCDFTrain$class,
    kernel = "radial",
    tunecontrol = tune.control(cross = 5),
    ranges = list(
      cost = c(0.001, 0.01, 0.1, 1, 2,
               10, 100, 1000),
      gamma = c(0.01, 0.1, 0.5, 1, 2)
    )
  )


ml.model.svm.jahoo <-
  e1071::svm(
    JahooMFCCDFTrain[, 2: (ncol(JahooMFCCDFTrain) -1)],
    JahooMFCCDFTrain$class,
    kernel = "radial",
    gamma = tune.rad$best.parameters$gamma,
    cost = tune.rad$best.parameters$cost,
    cross = 20,
    probability = TRUE
  )

ml.model.svm.jahoo$tot.accuracy


JahooTestMFCCDF <- MFCCFunction(input.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/Clips/CrestedGibbons/',
                                min.freq = 400,
                                max.freq = 3000,
                                n.windows = 9,
                                num.cep = 12,
                                win.avg = 'standard',
                                win.hop.time = 0.25)


JahooMFCCDFTestNoise <- MFCCFunction(input.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/Clips/Noise/',
                                     min.freq = 400,
                                     max.freq = 3000,
                                     n.windows = 9,
                                     num.cep = 12,
                                     win.avg = 'standard',
                                     win.hop.time = 0.25)

JahooMFCCDFTest <- rbind.data.frame(JahooTestMFCCDF,JahooMFCCDFTestNoise)

JahooMFCCDFTest$class <- as.factor(JahooMFCCDFTest$class )
table(JahooMFCCDFTest$class)

head(JahooMFCCDFTest)

TestPredictions <- predict( ml.model.svm.jahoo,JahooMFCCDFTest[, 2: (ncol(JahooMFCCDFTest) -1)], probability = TRUE)
TestPredictionsProb <- as.data.frame(attr(TestPredictions, "probabilities"))
TestPredictionsProb$ActualLabel <-JahooMFCCDFTest$class



# RF Test -----------------------------------------------------------------

# ml.model.svm.jahoo <-
#   randomForest::randomForest(x =  JahooMFCCDFTrain[, 2: (ncol(JahooMFCCDFTest) -1)], y = JahooMFCCDFTrain$class)
#
# TestPredictions <- predict( ml.model.svm.jahoo,JahooMFCCDFTest[, 2: (ncol(JahooMFCCDFTest) -1)],  type = 'prob')
# TestPredictionsProb <- as.data.frame(TestPredictions[,1:2])
# TestPredictionsProb$ActualLabel <-JahooMFCCDFTest$class


# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonSVM <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-TestPredictionsProb

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single[,1]  <=Thresholds[a], 'Noise','CrestedGibbons')

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
  # # Create a row for the result and add it to the BestF1data.frameCrestedGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonSVM <- rbind.data.frame(BestF1data.frameCrestedGibbonSVM, TempF1Row)
}

BestF1data.frameCrestedGibbonSVM
max(na.omit(BestF1data.frameCrestedGibbonSVM$F1))
BestF1data.frameCrestedGibbonSVM[which.max(na.omit(BestF1data.frameCrestedGibbonSVM$F1)),]


MaxF1SVM <- round(max(na.omit(BestF1data.frameCrestedGibbonSVM$F1)),2)
# Metric plot
CrestedGibbonSVMPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVM, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (SVM + MFCC) \n",'Max F1=', MaxF1SVM),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonSVMPlot


cowplot::plot_grid(CrestedGibbonSVMPlot,CrestedGibbonCNNBinary,CrestedGibbonCNNMulti,CrestedGibbonBirdNETPlot)

# GibbonR

TestFileDirectory <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/SoundFiles'

OutputDirectory <-  "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/gibbonR/"

gibbonR(input=TestFileDirectory,
        feature.df=JahooMFCCDFTrain,input.type='directory',
        model.type.list=c('SVM'),
        tune = TRUE,
        short.wav.duration=300,
        target.signal = c("Gibbons"),
        min.freq = 400, max.freq = 3000,
        noise.quantile.val=0.15,
        minimum.separation =3,
        n.windows = 9, num.cep = 12,
        spectrogram.window =160,
        pattern.split = ".wav",
        min.signal.dur = 3,
        max.sound.event.dur = 25,
        maximum.separation =1,
        probability.thresh.svm = 0.1,
        probability.thresh.rf = 0.15,
        wav.output = "TRUE",
        output.dir =OutputDirectory,
        swift.time=FALSE,time.start=5,time.stop=10,
        write.table.output=TRUE,verbose=TRUE,
        random.sample='NA')


