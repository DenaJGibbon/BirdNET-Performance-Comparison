# Focus only on small number of samples and allow early stopping
devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

# Train model over random samples ---------------------------------------------------------
ListRandomFolders <- list.files('/Volumes/DJC Files/JahooClipsRandomImagesSmall/',full.names = TRUE)

for(b in 1:length(ListRandomFolders)){

  # Location of spectrogram images for training
  input.data.path <-  ListRandomFolders[b]

  # Location of spectrogram images for testing
  test_data_path <- '/Volumes/DJC Files/imagesvietnam/test'

  # Training data folder short
  trainingfolder.short <- basename(ListRandomFolders[b])

  Nsamplenumeric <- as.numeric(str_split_fixed(trainingfolder.short,'samples',2)[,1])

  # Number of epochs to include
  epoch.iterations <- c(100)

  # Train the models specifying different architectures

  batch_size = round(Nsamplenumeric*0.3,0)


  freeze.param <- c(TRUE)

  gibbonNetR::train_CNN_binary(
    input.data.path = input.data.path,
    noise.weight = 0.5,
    architecture = 'resnet50',
    save.model = TRUE,
    learning_rate = 0.001,
    test.data = test_data_path,
    unfreeze.param = freeze.param,
    batch_size =batch_size,
    # FALSE means the features are frozen
    epoch.iterations = epoch.iterations,
    list.thresholds = seq(0, 1, .1),
    early.stop = "yes",
    output.base.path = "/Volumes/DJC Files/benchmark_results/gibbonNetR/",
    trainingfolder = trainingfolder.short,
    positive.class = "gibbon",
    negative.class = "noise"
  )


}


# Specify model path
ModelPath <- list.files('/Volumes/DJC Files/JahooGibbonModelsRandomGibbonNetR',full.names =TRUE,
                        recursive = TRUE)

ModelList <- ModelPath[str_detect(ModelPath,'.pt')]

for(k in 1:length(ModelList)){ tryCatch({
  print(k)
  ModelPath <- ModelList[k]
  ModelName <- str_split_fixed(basename(ModelList[k]),'_model',n=2)[,1]

  OutputFolder <- paste('/Volumes/DJC Files/benchmark_results/',ModelName,'/',sep='')

  WavFiles <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonIgnoreWindows'

  class_names <- c('gibbon','noise')
  trained_models_dir <-   '/Volumes/DJC Files/JahooGibbonModelsRandomGibbonNetR/_5samples_1_binary_unfrozen_TRUE_/'

    evaluate_trainedmodel_performance(trained_models_dir = trained_models_dir,
                                      image_data_dir = test_data_path,
                                      output_dir =OutputFolder,
                                      positive.class = 'gibbon',  #' Label for positive class
                                      negative.class = 'noise')    #' Label for negative class

  evaluate_trainedmodel_performance_multi(trained_models_dir=trained_models_dir,
                                             class_names=class_names,
                                             image_data_dir=test_data_path,
                                            output_dir=OutputFolder,
                                            noise.category = "noise")

  deploy_CNN_binary (
    clip_duration = 12,
    architecture='resnet50',
    output_folder = paste(OutputFolder,'/Images/',sep=''),
    output_folder_selections = paste(OutputFolder,'/Selections/',sep=''),
    output_folder_wav = paste(OutputFolder,'/Wavs/',sep=''),
    detect_pattern=NA,
    top_model_path = ModelPath,
    path_to_files = WavFiles,
    downsample_rate = 'NA',
    threshold = 0.1,
    save_wav = FALSE,
    positive.class = 'gibbon',
    negative.class = 'noise',
    max_freq_khz = 3
  )

}, error = function(e) {
  cat("ERROR :", conditionMessage(e), "\n")
})
}

