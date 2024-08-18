library(stringr)

# Match clips to human verified spectrograms

ImagesFile <- list.files('/Volumes/DJC Files/JahooArrayTrueFalsePositivesSorted/AlexNet27_09_22/Detections/FemaleDetectionsTP/',
                         recursive = T,full.names = T)

ImagesFileShort <- basename(ImagesFile)
ImagesFileShort <- str_split_fixed(ImagesFileShort,pattern = '.jpg', n=2)[,1]



WavFile <- list.files('/Volumes/DJC Files/JahooArrayTrueFalsePositivesSorted/AlexNet27_09_22/FemaleWavs/',
                      recursive = T,full.names = T)

WavFileShort <- basename(WavFile)
WavFileShort <- str_split_fixed(WavFileShort,pattern = '.wav', n=2)[,1]

OutputDir <- '/Volumes/DJC Files/JahooArrayTrueFalsePositivesSorted/SortedDetectionsWav/'

file.copy(
from=WavFile[which((WavFileShort %in% ImagesFileShort))],
to= paste(OutputDir,'CrestedGibbons/',WavFileShort[which((WavFileShort %in% ImagesFileShort))],'.wav',sep=''))


# Noise  ------------------------------------------------------------------

library(stringr)

# Match clips to human verified spectrograms

ImagesFile <- list.files('/Volumes/DJC Files/JahooArrayTrueFalsePositivesSorted/AlexNet27_09_22/Detections/FemaleDetectionsFP/',
                         recursive = T,full.names = T)

ImagesFileShort <- basename(ImagesFile)
ImagesFileShort <- str_split_fixed(ImagesFileShort,pattern = '.jpg', n=2)[,1]



WavFile <- list.files('/Volumes/DJC Files/JahooArrayTrueFalsePositivesSorted/AlexNet27_09_22/FemaleWavs/',
                      recursive = T,full.names = T)

WavFileShort <- basename(WavFile)
WavFileShort <- str_split_fixed(WavFileShort,pattern = '.wav', n=2)[,1]

OutputDir <- '/Volumes/DJC Files/JahooArrayTrueFalsePositivesSorted/SortedDetectionsWav/Noise/'

file.copy(
  from=WavFile[which((WavFileShort %in% ImagesFileShort))],
  to= paste(OutputDir,WavFileShort[which((WavFileShort %in% ImagesFileShort))],'.wav',sep=''))



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



# Images ignore windows ---------------------------------------------------


ImagesFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/ImagesIgnoreWindows/CrestedGibbons/',
                         recursive = T,full.names = T)

ImagesFileShort <- basename(ImagesFile)
ImagesFileShort <- str_split_fixed(ImagesFileShort,pattern = '.jpg', n=2)[,1]
ImagesFileShort <- stringi::stri_trim(ImagesFileShort)
#Splits <- str_split_fixed(ImagesFileShort,pattern = '_', n=5)

#ImagesFileShort <- paste(Splits[,1],Splits[,2],Splits[,3],sep='_')

WavFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/Clips/CrestedGibbons/',
                      recursive = T,full.names = T)

WavFileShort <- basename(WavFile)
WavFileShort <- str_split_fixed(WavFileShort,pattern = '.wav', n=2)[,1]

OutputDir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/ImagesIgnoreWindowsWavs/'

file.copy(
  from=WavFile[which((WavFileShort %in% ImagesFileShort))],
  to= paste(OutputDir,'CrestedGibbons/',WavFileShort[which((WavFileShort %in% ImagesFileShort))],'.wav',sep=''))


# Noise -------------------------------------------------------------------

ImagesFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/ImagesIgnoreWindows/Noise/',
                         recursive = T,full.names = T)

ImagesFileShort <- basename(ImagesFile)
ImagesFileShort <- str_split_fixed(ImagesFileShort,pattern = '.jpg', n=2)[,1]
ImagesFileShort <- stringi::stri_trim(ImagesFileShort)
#Splits <- str_split_fixed(ImagesFileShort,pattern = '_', n=5)

#ImagesFileShort <- paste(Splits[,1],Splits[,2],Splits[,3],sep='_')

WavFile <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/Clips/Noise/',
                      recursive = T,full.names = T)

WavFileShort <- basename(WavFile)
WavFileShort <- str_split_fixed(WavFileShort,pattern = '.wav', n=2)[,1]

OutputDir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/ImagesIgnoreWindowsWavs/'

file.copy(
  from=WavFile[which((WavFileShort %in% ImagesFileShort))],
  to= paste(OutputDir,'Noise/',WavFileShort[which((WavFileShort %in% ImagesFileShort))],'.wav',sep=''))


