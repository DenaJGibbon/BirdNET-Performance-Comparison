library(stringr)

# Match clips to human verified spectrograms

ImagesFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/Images/GreyGibbons/',
                         recursive = T,full.names = T)

ImagesFileShort <- basename(ImagesFile)
ImagesFileShort <- str_split_fixed(ImagesFileShort,pattern = '.jpg', n=2)[,1]



WavFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/Clips/',
                      recursive = T,full.names = T)

WavFileShort <- basename(WavFile)
WavFileShort <- str_split_fixed(WavFileShort,pattern = '.wav', n=2)[,1]

OutputDir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/ClipsManual/'

file.copy(
from=WavFile[which((WavFileShort %in% ImagesFileShort))],
to= paste(OutputDir,'GreyGibbons/',WavFileShort[which((WavFileShort %in% ImagesFileShort))],'.wav',sep=''))


# Noise  ------------------------------------------------------------------

library(stringr)

# Match clips to human verified spectrograms

ImagesFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/Images/Noise/',
                         recursive = T,full.names = T)

ImagesFileShort <- basename(ImagesFile)
ImagesFileShort <- str_split_fixed(ImagesFileShort,pattern = '.jpg', n=2)[,1]



WavFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/Clips/',
                      recursive = T,full.names = T)

WavFileShort <- basename(WavFile)
WavFileShort <- str_split_fixed(WavFileShort,pattern = '.wav', n=2)[,1]

OutputDir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/ClipsManual/'

file.copy(
  from=WavFile[which((WavFileShort %in% ImagesFileShort))],
  to= paste(OutputDir,'Noise/',WavFileShort[which((WavFileShort %in% ImagesFileShort))],'.wav',sep=''))


# Crested gibbons ---------------------------------------------------------

ImagesFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/Images/CrestedGibbons/',
                         recursive = T,full.names = T)

ImagesFileShort <- basename(ImagesFile)
ImagesFileShort <- str_split_fixed(ImagesFileShort,pattern = '.jpg', n=2)[,1]



WavFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/Clips/',
                      recursive = T,full.names = T)

WavFileShort <- basename(WavFile)
WavFileShort <- str_split_fixed(WavFileShort,pattern = '.wav', n=2)[,1]

OutputDir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/ClipsManual/'

file.copy(
  from=WavFile[which((WavFileShort %in% ImagesFileShort))],
  to= paste(OutputDir,'CrestedGibbons/',WavFileShort[which((WavFileShort %in% ImagesFileShort))],'.wav',sep=''))


# Crested gibbons - noise ---------------------------------------------------------

ImagesFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/Images/Noise/',
                         recursive = T,full.names = T)

ImagesFileShort <- basename(ImagesFile)
ImagesFileShort <- str_split_fixed(ImagesFileShort,pattern = '.jpg', n=2)[,1]



WavFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/Clips/',
                      recursive = T,full.names = T)

WavFileShort <- basename(WavFile)
WavFileShort <- str_split_fixed(WavFileShort,pattern = '.wav', n=2)[,1]

OutputDir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/ClipsManual/'

file.copy(
  from=WavFile[which((WavFileShort %in% ImagesFileShort))],
  to= paste(OutputDir,'Noise/',WavFileShort[which((WavFileShort %in% ImagesFileShort))],'.wav',sep=''))

