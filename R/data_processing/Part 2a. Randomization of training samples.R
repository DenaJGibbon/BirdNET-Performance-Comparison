# Specify the training folders
train.signal <- '/Volumes/DJC Files/Danum Deep Learning/MultiSpeciesAnalysis/JahooClips/Gibbons'
list.train.wavs <- list.files(train.signal,full.names = T)
signal.class <- 'gibbon'
length(list.train.wavs)

train.noise <- '/Volumes/DJC Files/Danum Deep Learning/MultiSpeciesAnalysis/JahooClips/Noise'
list.noise.wavs <- list.files(train.noise,full.names = T)
noise.class <- 'noise'
length(list.noise.wavs)

output.dir.random <- '/Volumes/DJC Files/JahooGibbonClipsRandomSmall/'

# Identify the maximum number of samples
Max.n <- 300

# Set the number of randomizations
Random.n <- 5

# Set the random seq
Random.seq <- seq(5,Max.n,5)

# Loop to randomly sample training data over specified iterations
for(c in 1:(Random.n)){

  for(d in 1:length(Random.seq)){
      print(paste('processing',c, 'out of', Random.n, 'for subset',Random.seq[d] ))
      subset.val <- Random.seq[d]
      signal.subset <- list.train.wavs[sample(1:length(list.train.wavs),subset.val,replace = F)]
      noise.subset <- list.noise.wavs[sample(1:length(list.noise.wavs),subset.val,replace = F)]


      subset.directory.signal <- paste(output.dir.random,Random.seq[d],'samples',
                                       '_',c,'/', signal.class,'/', sep='')

      if (!dir.exists(subset.directory.signal)){
        dir.create(subset.directory.signal,recursive = T)
        print(paste('Created output dir',subset.directory.signal))
      } else {
        print(paste(subset.directory.signal,'already exists'))
      }

      file.copy(signal.subset, subset.directory.signal)

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



