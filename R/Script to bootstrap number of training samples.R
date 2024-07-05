# What is the minimum number of calls to get decent performance?

# Danum Randomization -----------------------------------------------------

TrainingDataFilesDanumGibbons <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/TrainingDataWavs/DanumClipsBirdNET/train/Gibbons/',
                                    full.names = T )

TrainingDataFilesDanumNoise <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/TrainingDataWavs/DanumClipsBirdNET/train/noise/',
                                            full.names = T )

OutputDir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/BootstrappingDataSet/Danum/'

# Create dataset for random iterations ----------------------------------
subset.vals <- c(5,10,20,40,80,160)

# Loop to randomly sample training data over 10 iterations
    for(z in 1:10){
      for(a in 1:length(subset.vals)){
    print(paste('processing',z, 'out of 10 for subset',subset.vals[a] ))
    subset.val <- subset.vals[a]
    duet.subset <- TrainingDataFilesDanumGibbons[sample(1:length(TrainingDataFilesDanumGibbons),subset.val,replace = F)]
    noise.subset <- TrainingDataFilesDanumNoise[sample(1:length(TrainingDataFilesDanumNoise),subset.val,replace = F)]
    #combined.subset <- c(duet.subset,noise.subset)

    subset.directory.duet <- paste(OutputDir,'RandomizationSubset/Subset',subset.val,'_',z,'/gibbon/', sep='')

    if (!dir.exists(subset.directory.duet)){
      dir.create(subset.directory.duet,recursive = T)
      print(paste('Created output dir',subset.directory.duet))
    } else {
      print(paste(subset.directory.duet,'already exists'))
    }

    subset.directory.noise <- paste(OutputDir,'RandomizationSubset/Subset',subset.val,'_',z,'/noise/', sep='')

    if (!dir.exists(subset.directory.noise)){
      dir.create(subset.directory.noise,recursive = T)
      print(paste('Created output dir',subset.directory.noise))
    } else {
      print(paste(subset.directory.noise,'already exists'))
    }

    file.copy(duet.subset, subset.directory.duet)
    file.copy(noise.subset, subset.directory.noise)

      }
    }


# Jahoo Randomization -----------------------------------------------------


TrainingDataFilesJahooGibbons <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/TrainingDataWavs/JahooClipsBirdNET/train/Gibbons/',
                                            full.names = T )

TrainingDataFilesJahooNoise <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/TrainingDataWavs/JahooClipsBirdNET/train/noise/',
                                          full.names = T )

OutputDir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/BootstrappingDataSet/Jahoo/'

# Create dataset for random iterations ----------------------------------

# Loop to randomly sample training data over 10 iterations
for(z in 1:10){
  for(a in 1:length(subset.vals)){
    print(paste('processing',z, 'out of 10 for subset',subset.vals[a] ))
    subset.val <- subset.vals[a]
    duet.subset <- TrainingDataFilesJahooGibbons[sample(1:length(TrainingDataFilesJahooGibbons),subset.val,replace = F)]
    noise.subset <- TrainingDataFilesJahooNoise[sample(1:length(TrainingDataFilesJahooNoise),subset.val,replace = F)]
    #combined.subset <- c(duet.subset,noise.subset)

    subset.directory.duet <- paste(OutputDir,'RandomizationSubset/Subset',subset.val,'_',z,'/gibbon/', sep='')

    if (!dir.exists(subset.directory.duet)){
      dir.create(subset.directory.duet,recursive = T)
      print(paste('Created output dir',subset.directory.duet))
    } else {
      print(paste(subset.directory.duet,'already exists'))
    }

    subset.directory.noise <- paste(OutputDir,'RandomizationSubset/Subset',subset.val,'_',z,'/noise/', sep='')

    if (!dir.exists(subset.directory.noise)){
      dir.create(subset.directory.noise,recursive = T)
      print(paste('Created output dir',subset.directory.noise))
    } else {
      print(paste(subset.directory.noise,'already exists'))
    }

    file.copy(duet.subset, subset.directory.duet)
    file.copy(noise.subset, subset.directory.noise)

  }
}


