devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

RandomizationFolders <-
  list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/BootstrappingDataSet/Danum/RandomizationSubset',
             full.names = T)
ImageOutputDir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/BootstrappingDataSet/Danum/RandomizationSubsetImages/'

for(a in 1:length(RandomizationFolders)){
  print(a)
  ImageDir <-  paste(ImageOutputDir, basename(RandomizationFolders[a]),sep='')
  # Create spectrogram images
  gibbonNetR::spectrogram_images(
    trainingBasePath = RandomizationFolders[a],
    outputBasePath   = ImageDir,
    splits           = c(0.7, 0.3, 0),  # 70% training, 30% validation
    minfreq.khz = 0.4,
    maxfreq.khz = 2,
    new.sampleratehz= 'NA'
  )
}



RandomizationFolders <-
  list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/BootstrappingDataSet/Jahoo/RandomizationSubset',
             full.names = T)
ImageOutputDir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/BootstrappingDataSet/Jahoo/RandomizationSubsetImages/'

for(a in 1:length(RandomizationFolders)){
  print(a)
  ImageDir <-  paste(ImageOutputDir, basename(RandomizationFolders[a]),sep='')
  # Create spectrogram images
  gibbonNetR::spectrogram_images(
    trainingBasePath = RandomizationFolders[a],
    outputBasePath   = ImageDir,
    splits           = c(0.7, 0.3, 0),  # 70% training, 30% validation
    minfreq.khz = 0.4,
    maxfreq.khz = 3,
    new.sampleratehz= 'NA'
  )
}
