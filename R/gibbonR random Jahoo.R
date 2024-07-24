library(ggpubr)

devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonR")

RandomFolders <- list.files('/Volumes/DJC Files/JahooGibbonClipsRandom', full.names = TRUE)

for(i in 1:length(RandomFolders)){

SampleFolders <- list.files(RandomFolders[i],full.names = TRUE)

DanumMFCCDF <- MFCCFunction(input.dir=SampleFolders[1],
                            min.freq = 500,
                            max.freq = 3000,
                            n.windows = 9,
                            num.cep = 12,
                            win.avg = 'standard',
                            win.hop.time = 0.25)


DanumMFCCDFNoise <- MFCCFunction(input.dir=SampleFolders[2],
                                 min.freq = 500,
                                 max.freq = 3000,
                                 n.windows = 9,
                                 num.cep = 12,
                                 win.avg = 'standard',
                                 win.hop.time = 0.25)



DanumMFCCDFTrain <- rbind.data.frame(DanumMFCCDF,DanumMFCCDFNoise)

DanumMFCCDFTrain$class <- as.factor(DanumMFCCDFTrain$class )

TestFileDirectory <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/SoundFiles/'

OutputDirectory <- paste('/Volumes/DJC Files/JahooGibbonModelsRandomgibbonR/',basename(RandomFolders[i]),sep='')
dir.create(OutputDirectory,recursive = TRUE)


gibbonR(input=TestFileDirectory,
        input.type='directory',
        feature.df=DanumMFCCDFTrain,
        model.type.list=c('SVM'),
        tune = TRUE,
        short.wav.duration=300,
        target.signal = c("Gibbons"),
        min.freq = 500, max.freq = 3000,
        noise.quantile.val=0.15,
        #time.window.number =3,
        n.windows = 9, num.cep = 12,
        spectrogram.window =160,
        pattern.split = ".wav",
        min.signal.dur = 3,
        max.sound.event.dur = 25,
        maximum.separation =1,
        probability.thresh.svm = 0.15,
        probability.thresh.rf = 0.15,
        wav.output = "FALSE",
        output.dir =OutputDirectory,
        swift.time=FALSE,time.start=5,time.stop=10,
        write.table.output=TRUE,verbose=TRUE,
        random.sample='NA')

}


