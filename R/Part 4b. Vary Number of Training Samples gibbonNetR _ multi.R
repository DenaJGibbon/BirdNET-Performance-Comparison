
# Load necessary packages and functions
devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

TempFolder <- list.files('/Volumes/DJC Files/JahooGibbonClipsRandomSmallMulti',full.names = T)
OutputDir <- '/Volumes/DJC Files/JahooClipsRandomImagesMulti/'

for(a in 8:length(TempFolder)){

TempPath <-  basename(TempFolder[a])
# Create spectrogram images
spectrogram_images(
  trainingBasePath = TempFolder[a],
  outputBasePath = paste(OutputDir,TempPath, sep=''),
  minfreq.khz = 0.5,
  maxfreq.khz = 3.0,
  splits = c(0.7, 0.3, 0), # Assign proportion to training, validation, or test folders
  new.sampleratehz = 16000
)

}

# Train model over random samples ---------------------------------------------------------
ListRandomFolders <- list.files('/Volumes/DJC Files/JahooClipsRandomImagesMulti',full.names = TRUE)
for(b in c(1:5,12:16)){

# Location of spectrogram images for training
input.data.path <-  ListRandomFolders[b]

# Location of spectrogram images for testing
test_data_path <- '/Volumes/DJC Files/imagesvietnam/test_gibbonnetr'

# Training data folder short
trainingfolder.short <- basename(ListRandomFolders[b])

Nsamplenumeric <- as.numeric(str_split_fixed(trainingfolder.short,'samples',2)[,1])

# Number of epochs to include
epoch.iterations <- c(5)

# Train the models specifying different architectures

batch_size = round(Nsamplenumeric*0.3,0)

freeze.param <- c(TRUE)

# Train models using different architectures
gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture ='resnet50',
                            learning_rate = 0.001,
                            test.data=test_data_path,
                            batch_size =batch_size,
                            #list.thresholds = seq(0, 1, .1),
                            class_weights = c(0.33, 0.33, 0.33),
                            unfreeze.param = TRUE,
                            epoch.iterations=epoch.iterations,
                            save.model= TRUE,
                            early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/JahooGibbonModelsRandomGibbonNetRmulti/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "noise")


}


# Deploy model over sound files -------------------------------------------

# Specify model path
   ModelPath <- list.files('/Volumes/DJC Files/JahooGibbonModelsRandomGibbonNetRmulti',full.names =TRUE,
                           recursive = TRUE)

   ModelList <- ModelPath[str_detect(ModelPath,'.pt')]

   for(k in 28:37){ tryCatch({
   print(k)
   ModelPath <- ModelList[k]
   ModelName <- str_split_fixed(basename(ModelList[k]),'_model',n=2)[,1]

   OutputFolder <- paste('/Volumes/DJC Files/JahooTestDataPerformancegibbonNetRmulti/',ModelName,'/',sep='')

   WavFiles <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/SoundFiles'

   deploy_CNN_multi(
     clip_duration = 12,
     architecture='resnet50',
     output_folder = paste(OutputFolder,'/Images/',sep=''),
     output_folder_selections = paste(OutputFolder,'/Selections/',sep=''),
     output_folder_wav = paste(OutputFolder,'/Wavs/',sep=''),
     detect_pattern=NA,
     top_model_path = ModelPath,
     path_to_files =WavFiles,
     downsample_rate = 'NA',
     save_wav = F,
     class_names = c('CrestedGibbons','GreyGibbons','noise'),
     noise_category = 'noise',
     single_class = TRUE,
     single_class_category = 'CrestedGibbons',
     threshold = .1,
     max_freq_khz = 3
   )


   }, error = function(e) {
     cat("ERROR :", conditionMessage(e), "\n")
   })
}



# Add true/false positives ------------------------------------------------

   spectrogram_images(
     trainingBasePath = '/Volumes/DJC Files/JahooGibbonClipsRandomSmallMulti/1085samples_1',
     outputBasePath = paste(OutputDir,'1085samples_1', sep=''),
     minfreq.khz = 0.5,
     maxfreq.khz = 3.0,
     splits = c(0.7, 0.3, 0), # Assign proportion to training, validation, or test folders
     new.sampleratehz = 16000
   )



# train multi -------------------------------------------------------------

   # Location of spectrogram images for training
   input.data.path <-   "/Volumes/DJC Files/JahooClipsRandomImagesMulti/1085samples_1"

   # Location of spectrogram images for testing
   test_data_path <- '/Volumes/DJC Files/imagesvietnam/test_gibbonnetr'

   # Training data folder short
   trainingfolder.short <-"1085samples_2"

   Nsamplenumeric <- as.numeric(str_split_fixed(trainingfolder.short,'samples',2)[,1])

   # Number of epochs to include
   epoch.iterations <- c(3)

   # Train the models specifying different architectures

   batch_size = round(Nsamplenumeric*0.3,0)

   freeze.param <- c(TRUE)

   # Train models using different architectures
   gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                               architecture ='resnet50',
                               learning_rate = 0.001,
                               test.data=test_data_path,
                               batch_size =batch_size,
                               #list.thresholds = seq(0, 1, .1),
                               class_weights = c(0.33, 0.33, 0.33),
                               unfreeze.param = TRUE,
                               epoch.iterations=epoch.iterations,
                               save.model= TRUE,
                               early.stop = "yes",
                               output.base.path = "/Volumes/DJC Files/JahooGibbonModelsRandomGibbonNetRmulti/",
                               trainingfolder=trainingfolder.short,
                               noise.category = "noise")


   ModelPath <- list.files('/Volumes/DJC Files/JahooGibbonModelsRandomGibbonNetRmulti/_1085samples_1_multi_unfrozen_TRUE_',
                           full.names = T)
   ModelName <- str_split_fixed(basename(ModelPath[2]),'_model',n=2)[,1]

   ModelName <- "_1085samples_2_5_resnet50"
   OutputFolder <- paste('/Volumes/DJC Files/JahooTestDataPerformancegibbonNetRmulti/',ModelName,'/',sep='')

   WavFiles <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/SoundFiles'

   deploy_CNN_multi(
     clip_duration = 12,
     architecture='resnet50',
     output_folder = paste(OutputFolder,'/Images/',sep=''),
     output_folder_selections = paste(OutputFolder,'/Selections/',sep=''),
     output_folder_wav = paste(OutputFolder,'/Wavs/',sep=''),
     detect_pattern=NA,
     top_model_path = ModelPath[2],
     path_to_files =WavFiles,
     downsample_rate = 'NA',
     save_wav = F,
     class_names = c('CrestedGibbons','GreyGibbons','noise'),
     noise_category = 'noise',
     single_class = TRUE,
     single_class_category = 'CrestedGibbons',
     threshold = .1,
     max_freq_khz = 3
   )

