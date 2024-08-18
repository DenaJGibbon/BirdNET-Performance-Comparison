
library(phonTools)
library(tuneR)
library(seewave)

# Note need to read in the function using 'run'
#source('/Users/denaclink/Desktop/RStudioProjects/BirdNET-Performance-Comparison/R/plot.spectrogram.R')

WavPath <- '/Users/denaclink/Raven Pro 1.6/JahooExample/CrestedExample_R1023_20220320_050002.wav'

TempWav <- readWave(WavPath)
TempSpec <- spectrogram (TempWav@left,fs = TempWav@samp.rate,windowlength = 75, quality = F,maxfreq = 5000)

tiff('Spectroexamplefullduet.tiff',width = 1600,res=200,height = 800)
plot.spectrogram(TempSpec)
graphics.off()


SelectionTable <- read.delim('/Users/denaclink/Desktop/RStudioProjects/Jahoo-Gibbon-Calls/data/SelectionTables_09012023/Hope_R1060_20220830_060002.Table.1.selections.txt')
WavFile <- readWave('/Users/denaclink/Library/CloudStorage/Box-Box/Cambodia 2022/Acoustic Gibbon PAM 27_09_22/SD25/R1060_000/R1060_2022-08-30/R1060_20220830_060002.wav')

TempWavLQ <- cutw(WavFile,from =SelectionTable$Begin.Time..s.[2],to=SelectionTable$End.Time..s.[2],output = 'Wave' )
TempWavMQ <- readWave('/Volumes/DJC Files/JahooGibbonClipsRandom/5samples_1/gibbon/Gibbons_M_R1023_20220502_090003_2617.wav')
TempWavHQ <- readWave('/Volumes/DJC Files/JahooGibbonClipsRandom/5samples_1/gibbon/Gibbons_H_R1023_20220320_050002_2893.wav')

TempSpecLQ <- spectrogram (TempWavLQ@left,fs = TempWav@samp.rate,windowlength = 75, quality = F)
TempSpecMQ <-spectrogram (TempWavMQ@left,fs = TempWav@samp.rate,windowlength = 75, quality = F)
TempSpecHQ <-spectrogram (TempWavHQ@left,fs = TempWav@samp.rate,windowlength = 75, quality = F)

tiff('SpectroexampleLQ1.tiff',width = 1600,res=200)
par(mfrow=c(1,3))
plot.spectrogram(TempSpecLQ)
plot.spectrogram(TempSpecMQ)
plot.spectrogram(TempSpecHQ)
graphics.off()

