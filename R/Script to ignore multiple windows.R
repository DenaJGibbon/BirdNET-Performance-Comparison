library(stringr)

# Script to move images
OutputDir <-'/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/ImagesIgnoreWindows/'

ImageFiles <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/Images/CrestedGibbons',
                         full.names = T)

ImageFilesShort <- basename(ImageFiles)
ImageFilesShort <- str_split_fixed(ImageFilesShort,
                                   pattern = '.jpg',
                                   n=2)[,1]

TempSplitName <- str_split_fixed(ImageFilesShort,
                pattern = '_',
                n=6)


FileName <- paste(TempSplitName[,2],
                  TempSplitName[,3],
                  TempSplitName[,4],
                  TempSplitName[,5], sep='_'
                  )

Time <- TempSplitName[,6]

UniqueFileNames <- unique(FileName)

for(a in 1:length(UniqueFileNames)){

 Index <-  which(FileName %in% UniqueFileNames[a])
 ImageFilesFull <-  ImageFiles[Index]
 FileNameSub <- FileName[Index]
 TimeSub <- as.numeric(Time[Index])

 TimeList <- list()
 for(b in 1: (length(Index)-1) ) {

   TimeList[[b]] <-  TimeSub[b+1]  - TimeSub[b]

 }

 TimeList[[length(Index)]] <- 0

 IndexRemove <- which(unlist(TimeList)==6)

 file.copy(from=ImageFilesFull[-IndexRemove],
           to=paste(OutputDir,
                    basename(ImageFilesFull)[-IndexRemove] ))

}


# Danum Valley ------------------------------------------------------------

library(stringr)

# Script to move images
OutputDir <-'/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/ImagesIgnoreWindows/'

ImageFiles <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/Images/GreyGibbons',
                         full.names = T)

ImageFilesShort <- basename(ImageFiles)
ImageFilesShort <- str_split_fixed(ImageFilesShort,
                                   pattern = '.jpg',
                                   n=2)[,1]

TempSplitName <- str_split_fixed(ImageFilesShort,
                                 pattern = '_',
                                 n=6)


FileName <- paste(TempSplitName[,2],
                  TempSplitName[,3],
                  TempSplitName[,4], sep='_'
)

Time <- TempSplitName[,5]

UniqueFileNames <- unique(FileName)

for(a in 1:length(UniqueFileNames)){

  Index <-  which(FileName %in% UniqueFileNames[a])
  ImageFilesFull <-  ImageFiles[Index]
  FileNameSub <- FileName[Index]
  TimeSub <- as.numeric(Time[Index])

  TimeList <- list()
  for(b in 1: (length(Index)-1) ) {

    TimeList[[b]] <-  TimeSub[b+1]  - TimeSub[b]

  }

  TimeList[[length(Index)]] <- 0

  IndexRemove <- which(unlist(TimeList)==6)

  file.copy(from=ImageFilesFull[-IndexRemove],
            to=paste(OutputDir,
                     basename(ImageFilesFull)[-IndexRemove] ))

}

#NOTE that on April 8 DJC manually moved some misclassified images in the test dataset
setwd("/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies")

devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

# Top model for Crested Gibbons -------------------------------------------

trained_models_dir <- '/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/model_output/top_models/cambodia_binary/'

image_data_dir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/ImagesIgnoreWindows/'

evaluate_trainedmodel_performance(trained_models_dir=trained_models_dir,
                                  image_data_dir=image_data_dir,
                                  output_dir = "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_ImagesIgnoreWindows/",
                                  positive.class='CrestedGibbons')


PerformanceOutPutTrained <- gibbonNetR::get_best_performance(performancetables.dir= "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_ImagesIgnoreWindows/performance_tables_trained/",
                                                             model.type = 'binary',class='CrestedGibbons',Thresh.val =0.1)

PerformanceOutPutTrained$f1_plot
PerformanceOutPutTrained$best_f1$F1
PerformanceOutPutTrained$pr_plot
(PerformanceOutPutTrained$pr_plot)+scale_color_manual(values=matlab::jet.colors(6))


# Top model for Grey Gibbons -------------------------------------------

trained_models_dir <- '/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/model_output/top_models/malaysia_binary/'

image_data_dir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/ImagesIgnoreWindows/'

evaluate_trainedmodel_performance(trained_models_dir=trained_models_dir,
                                  image_data_dir=image_data_dir,
                                  output_dir = "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/model_eval_ImagesIgnoreWindows/",
                                  positive.class='GreyGibbons')


PerformanceOutPutTrained <- gibbonNetR::get_best_performance(performancetables.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/model_eval_ImagesIgnoreWindows/performance_tables_trained/',
                                                             model.type = 'binary',class='Gibbons',Thresh.val =0.1)

PerformanceOutPutTrained$f1_plot
PerformanceOutPutTrained$best_f1$F1
PerformanceOutPutTrained$pr_plot
(PerformanceOutPutTrained$pr_plot)+scale_color_manual(values=matlab::jet.colors(6))

# Cambodia multi ----------------------------------------------------------

trained_models_dir <- "/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/model_output/top_models/combined_multi/"


#image_data_dir <- '/Volumes/DJC 1TB/VocalIndividualityClips/RandomSelectionImages/'
image_data_dir <- "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/ImagesIgnoreWindows/"

trainingfolder <- 'imagesmulti'

class_names <- c("CrestedGibbons", "GreyGibbons", "Noise")

evaluate_trainedmodel_performance_multi(trained_models_dir=trained_models_dir,
                                        class_names=class_names,
                                        #trainingfolder=trainingfolder,
                                        image_data_dir=image_data_dir,
                                        output_dir="/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_ImagesIgnoreWindows_multi/",
                                        noise.category = "Noise")



PerformanceOutPutTrained <- gibbonNetR::get_best_performance(performancetables.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_ImagesIgnoreWindows_multi/performance_tables_multi_trained',
                                                             model.type = 'multi',
                                                             class='CrestedGibbons',Thresh.val =0.1)

PerformanceOutPutTrained$f1_plot
PerformanceOutPutTrained$best_f1$F1
PerformanceOutPutTrained$pr_plot

(PerformanceOutPutTrained$pr_plot)+scale_color_manual(values=matlab::jet.colors(6))


trained_models_dir <- "/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/model_output/top_models/combined_multi/"

#image_data_dir <- '/Volumes/DJC 1TB/VocalIndividualityClips/RandomSelectionImages/'
image_data_dir <- "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/ImagesIgnoreWindows/"

trainingfolder <- 'imagesmulti'

class_names <- c("CrestedGibbons", "GreyGibbons", "Noise")

evaluate_trainedmodel_performance_multi(trained_models_dir=trained_models_dir,
                                        class_names=class_names,
                                        #trainingfolder=trainingfolder,
                                        image_data_dir=image_data_dir,
                                        output_dir="/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/model_eval_ImagesIgnoreWindows_multi/",
                                        noise.category = "Noise")



PerformanceOutPutTrained <- gibbonNetR::get_best_performance(performancetables.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/model_eval_ImagesIgnoreWindows_multi/performance_tables_multi_trained',
                                                             model.type = 'multi',
                                                             class='GreyGibbons',Thresh.val =0.1)

PerformanceOutPutTrained$f1_plot
PerformanceOutPutTrained$best_f1$F1
PerformanceOutPutTrained$pr_plot
