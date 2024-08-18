# Specify the training folders
train.crestedsignal <- '/Volumes/DJC Files/Danum Deep Learning/MultiSpeciesAnalysis/JahooClips/Gibbons'
list.train.crestedwavs <- list.files(train.crestedsignal,full.names = T)
crested.class <- 'CrestedGibbon'
length(list.train.crestedwavs)

# Specify the training folders
train.greysignal <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/TrainingDataWavs/CombinedClips/train/GreyGibbons/'
list.train.greywavs <- list.files(train.greysignal,full.names = T)
grey.class <- 'GreyGibbon'
length(list.train.greywavs)

train.noise <- '/Volumes/DJC Files/Danum Deep Learning/MultiSpeciesAnalysis/JahooClips/Noise'
list.noise.wavs <- list.files(train.noise,full.names = T)
noise.class <- 'noise'
length(list.noise.wavs)

output.dir.random <- '/Volumes/DJC Files/BirdNETMissing/'

# Set the number of randomizations
Random.n <- 5

# Set the random seq
Random.seq <-c(165,185 )

# Loop to randomly sample training data over specified iterations
for(c in 1:(Random.n)){

  for(d in 1:length(Random.seq)){
      print(paste('processing',c, 'out of', Random.n, 'for subset',Random.seq[d] ))
      subset.val <- Random.seq[d]
      crested.subset <- list.train.crestedwavs[sample(1:length(list.train.crestedwavs),subset.val,replace = F)]
      grey.subset <- list.train.greywavs[sample(1:length(list.train.greywavs),subset.val,replace = F)]
      noise.subset <- list.noise.wavs[sample(1:length(list.noise.wavs),subset.val,replace = F)]


      crested.directory.signal <- paste(output.dir.random,Random.seq[d],'samples',
                                       '_',c,'/',crested.class,'/', sep='')

      if (!dir.exists(crested.directory.signal)){
        dir.create(crested.directory.signal,recursive = T)
        print(paste('Created output dir',crested.directory.signal))
      } else {
        print(paste(crested.directory.signal,'already exists'))
      }

      file.copy(crested.subset, crested.directory.signal)

      grey.directory.signal <- paste(output.dir.random,Random.seq[d],'samples',
                                        '_',c,'/',grey.class,'/', sep='')

      if (!dir.exists(grey.directory.signal)){
        dir.create(grey.directory.signal,recursive = T)
        print(paste('Created output dir',grey.directory.signal))
      } else {
        print(paste(grey.directory.signal,'already exists'))
      }

      file.copy(grey.subset, grey.directory.signal)



      subset.directory.noise <- paste(output.dir.random,Random.seq[d],'samples',
                                      '_',c,'/', noise.class,'/', sep='')

      if (!dir.exists(subset.directory.noise)){
        dir.create(subset.directory.noise,recursive = T)
        print(paste('Created output dir',subset.directory.noise))
      } else {
        print(paste(subset.directory.noise,'already exists'))
      }

      file.copy(noise.subset, subset.directory.noise)

  }
}



