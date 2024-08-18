library(stringr)
library(tuneR)
library(seewave)

ListSelectionTables <- list.files('/Users/denaclink/Downloads/JahooAnthropogenic',recursive=T,
           full.names = T)

CombinedDF <- data.frame()
# Combine in table
for( a in 1: length(ListSelectionTables)){
  print(paste('processing', a, 'out of',length(ListSelectionTables)))
  TempSelection <- read.delim( ListSelectionTables[a])
  if(nrow(TempSelection)> 0){
  TempSelection$Filename <- basename(ListSelectionTables[a])
  CombinedDF <- rbind.data.frame(CombinedDF,TempSelection )
}
}

CombinedDF$WavName<- str_split_fixed(CombinedDF$Filename, pattern ='.BirdNET', n=2)[,1]
head(CombinedDF)

CombinedDF

WavFiles <- list.files('/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustic Gibbon PAM_09_11_23',
           recursive = T, full.names = T, pattern = '.wav')

for(b in 4:nrow(CombinedDF)){
  print(b)
  TempRow <- CombinedDF[b,]
  TempWav <- WavFiles[which(str_detect(string=WavFiles,pattern=TempRow$WavName))]
  TempWav <- readWave(TempWav)

  ShortWav <- cutw(TempWav,from=TempRow$Begin.Time..s.-3,to=TempRow$End.Time..s.,output = 'Wave')
  FileName <- paste( c(TempRow[,c(9,4,7,8,12)]), collapse ='_')
  FileName <-paste('data/detections/',FileName,'.wav',sep='')
  writeWave(ShortWav,FileName,extensible = F)
}

WavList <- list.files('data/detections/',full.names = T)

for(c in 1:length(WavList)){

jpeg_filename <-
  paste('data/detection_images/',basename(WavList[c]),'.jpg',sep='')

jpeg(jpeg_filename, res = 50)

short_wav <- tuneR::readWave(WavList[c])

seewave::spectro(
  short_wav,
  tlab = '',
  flab = '',
  axisX = FALSE,
  axisY = FALSE,
  scale = FALSE,
  flim = c(0, 2),
  grid = FALSE
)
graphics.off()
}

PossibleList <- list.files('data/detection_images/TP/',full.names = F)

for(d in 1:length(PossibleList)){
  TempWav <- str_split_fixed(PossibleList[d],pattern = '.jpg',n=2)[,1]
  WaveName <- paste('data/detections/',TempWav,sep='')
  file.copy(from=WaveName,to=paste('data/possiblegunshots/',TempWav,sep=''))
}
