library(tidyr)
library(dplyr)
library(ggpubr)

# This is for automated detection -----------------------------------------

CombinedRandomizationDF <- read.csv('data/CombinedRandomizationAutomatedDetection.csv')
CombinedRandomizationDF$AUC <- CombinedRandomizationDF$auc
CombinedRandomizationDFSub <- CombinedRandomizationDF[,c('Model', 'F1','AUC','samples')]


MaxCombinedRandomizationDF <- CombinedRandomizationDFSub %>%
  dplyr::group_by(Model,samples,AUC) %>%
  dplyr::summarise(F1 = max(F1, na.rm=TRUE))

CombinedRandomizationDFlong<- MaxCombinedRandomizationDF %>%
  select(Model,F1, AUC, samples) %>%
  pivot_longer(cols = c(F1, AUC), names_to = "metric", values_to = "value")

CombinedRandomizationDFlong$value <- as.numeric(CombinedRandomizationDFlong$value)
CombinedRandomizationDFlong$samples <- as.numeric(CombinedRandomizationDFlong$samples)


ggpubr::ggline(data=CombinedRandomizationDFlong,
               x='samples',y='value',color='Model', add = "mean_se",facet.by = 'metric')+
                  ylim(0,1)+xlab('Number of samples')+ylab('Metric')+
                    scale_color_manual(values= c("#0080FF", "#00FFFF", "#80FF80", "#FF8000") )


