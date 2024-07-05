# devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")
#
# TrainingDatapath <- '/Volumes/DJC Files/GunshotDataWavBelize/Training data reduced'
#
# gibbonNetR::spectrogram_images(
#   trainingBasePath = TrainingDatapath,
#   outputBasePath   = 'data/imagesbelize/',
#   splits           = c(0.7, 0.3, 0),  # 70% training, 30% validation
#   minfreq.khz = 0,
#   maxfreq.khz = 2,
#   new.sampleratehz= 'NA'
# )


# A. Vietnam Binary Model Training (unbalanced) ---------------------------------------------------------
setwd("/Users/denaclink/Desktop/RStudioProjects/BirdNET-Performance-Comparison")

# Load necessary packages and functions
devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

# Location of spectrogram images for training
input.data.path <-  'data/imagesbelize/'

# Location of spectrogram images for testing
test_data_path <- '/Users/denaclink/Desktop/RStudioProjects/CleanGunshot/data/imagesbelizetest/test/'

# Training data folder short
trainingfolder.short <- 'imagesbelize'

# Number of epochs to include
epoch.iterations <- c(1, 2, 3, 20)

# Train the models specifying different architectures
architectures <- c('alexnet', 'vgg16', 'resnet18', 'resnet50','resnet152')
freeze.param <- c(FALSE)
for (a in 1:length(architectures)) {
  for (b in 1:length(freeze.param)) {
    gibbonNetR::train_CNN_binary(
      input.data.path = input.data.path,
      noise.weight = 0.25,
      architecture = architectures[a],
      save.model = TRUE,
      learning_rate = 0.001,
      test.data = test_data_path,
      unfreeze.param = freeze.param[b],
      # FALSE means the features are frozen
      epoch.iterations = epoch.iterations,
      list.thresholds = seq(0, 1, .1),
      early.stop = "yes",
      output.base.path = "/Users/denaclink/Desktop/RStudioProjects/BirdNET-Performance-Comparison/BelizeBenchmark/",
      trainingfolder = trainingfolder.short,
      positive.class = "Gunshot",
      negative.class = "noise"
    )

  }
}

performancetables.dir.multi.balanced.true <-'/Users/denaclink/Desktop/RStudioProjects/BirdNET-Performance-Comparison/BelizeBenchmark/_imagesbelize_binary_unfrozen_FALSE_/performance_tables'

PerformanceOutputmulti.balanced.true <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir.multi.balanced.true,
                                                                         class='Gunshot',
                                                                         model.type = "binary",
                                                                         Thresh.val = 0.1)

PerformanceOutputmulti.balanced.true$f1_plot
PerformanceOutputmulti.balanced.true$best_f1$F1

