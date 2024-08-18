library(stringr)

ModelFolders <- list.files('/Volumes/DJC Files/JahooGibbonModelsRandomGibbonNetR',
           full.names = TRUE)

CombinedDFclipsbinaryCNN <- data.frame()
for(a in 1:length(ModelFolders)){
  print(a)
 Temp.CSV <-  list.files(list.files(ModelFolders[a],full.names = T)[4],full.names = T)
 TempVals <- read.csv(Temp.CSV)
 TempName <-basename(Temp.CSV)
 TempVals$samples <-  as.numeric(str_split_fixed(TempName,'samples',n=2)[,1])
 CombinedDFclipsbinaryCNN <- rbind.data.frame(CombinedDFclipsbinaryCNN,TempVals )
}

CombinedDFclipsbinaryCNN <- na.omit(CombinedDFclipsbinaryCNN)
CombinedDFclipsbinaryCNN$Precision <- round(CombinedDFclipsbinaryCNN$Precision,1)
CombinedDFclipsbinaryCNN$Recall <- round(CombinedDFclipsbinaryCNN$Recall,2)
CombinedDFclipsbinaryCNN$F1 <- as.numeric(round(CombinedDFclipsbinaryCNN$F1,2))

MaxF1PlotCNNBinary <- CombinedDFclipsbinaryCNN %>%
  dplyr::group_by(Training.Data) %>%
  dplyr::summarise(F1 = max(F1, na.rm=TRUE))

MaxF1PlotCNNBinary$samples <-  as.numeric(str_split_fixed(MaxF1PlotCNNBinary$Training.Data,'samples',n=2)[,1])
ggpubr::ggline(data=MaxF1PlotCNNBinary,x='samples',y='F1',add = "mean_se")+ylim(0,1)+ggtitle('ResNet50 binary')


ModelFolders <- list.files('/Volumes/DJC Files/JahooGibbonModelsRandomGibbonNetRmulti',
                           full.names = TRUE)

CombinedDFclipsmultiCNN <- data.frame()
for(a in 1:length(ModelFolders)){
  print(a)
  Temp.CSV <-  list.files(list.files(ModelFolders[a],full.names = T)[4],full.names = T)
  TempVals <- read.csv(Temp.CSV)
  TempName <-basename(Temp.CSV)
  TempVals$samples <-  as.numeric(str_split_fixed(TempName,'samples',n=2)[,1])
  CombinedDFclipsmultiCNN <- rbind.data.frame(CombinedDFclipsmultiCNN,TempVals )
}

CombinedDFclipsmultiCNN <- na.omit(CombinedDFclipsmultiCNN)
CombinedDFclipsmultiCNN$Precision <- round(CombinedDFclipsmultiCNN$Precision,1)
CombinedDFclipsmultiCNN$Recall <- round(CombinedDFclipsmultiCNN$Recall,2)
CombinedDFclipsmultiCNN$F1 <- as.numeric(round(CombinedDFclipsmultiCNN$F1,2))

MaxF1PlotCNNMulti <- CombinedDFclipsmultiCNN %>%
  dplyr::group_by(Training.Data) %>%
  dplyr::summarise(F1 = max(F1, na.rm=TRUE))

MaxF1PlotCNNMulti$samples <-  as.numeric(str_split_fixed(MaxF1PlotCNNMulti$Training.Data,'samples',n=2)[,1])
ggpubr::ggline(data=MaxF1PlotCNNMulti,x='samples',y='F1',add = "mean_se")+ylim(0,1)+ggtitle('ResNet50 multi')

MaxF1PlotBirdNETbinary$Model  <- 'BirdNET Binary'
MaxF1PlotBirdNETmulti$Model  <- 'BirdNET Multi'
MaxF1PlotCNNMulti$Model <- 'ResNet50 Multi'
MaxF1PlotCNNBinary$Model <- 'ResNet50 Binary'

colnames(MaxF1PlotCNNBinary) <- c("performancefolder", "F1", "samples", "Model")
colnames(MaxF1PlotCNNMulti) <- c("performancefolder", "F1", "samples", "Model")

CombinedPerformanceDF <- rbind.data.frame(MaxF1PlotCNNMulti,MaxF1PlotCNNBinary,
                                          MaxF1PlotBirdNETbinary,MaxF1PlotBirdNETmulti)

ggpubr::ggline(data=CombinedPerformanceDF,x='samples',y='F1', color='Model',
               add = "mean_se")+ylim(0,1)+xlab('Number of training samples')+
                scale_color_manual(values= c("#0080FF", "#00FFFF", "#80FF80", "#FF8000") )

