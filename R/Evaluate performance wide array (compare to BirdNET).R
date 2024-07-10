setwd("/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies")

devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

# Top model for Crested Gibbons -------------------------------------------

trained_models_dir <- '/Users/denaclink/Desktop/RStudioProjects/model_output/resnet_binary_cambodia/'

image_data_dir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/ImagesIgnoreWindowsWavs/'

evaluate_trainedmodel_performance(trained_models_dir=trained_models_dir,
                                  image_data_dir=image_data_dir,
                                  output_dir = "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_all/",
                                  positive.class='CrestedGibbons')


PerformanceOutPutTrained <- gibbonNetR::get_best_performance(performancetables.dir= "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_all/performance_tables_trained/",
                                                             model.type = 'binary',class='Gibbons',Thresh.val =0.1)

PerformanceOutPutTrained$f1_plot
PerformanceOutPutTrained$best_f1$F1
PerformanceOutPutTrained$pr_plot
(PerformanceOutPutTrained$pr_plot)+scale_color_manual(values=matlab::jet.colors(6))


# Top model for Grey Gibbons -------------------------------------------

trained_models_dir <- '/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/model_output/top_models/malaysia_binary/'

image_data_dir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/Images/'

evaluate_trainedmodel_performance(trained_models_dir=trained_models_dir,
                                  image_data_dir=image_data_dir,
                                  output_dir = "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/model_eval/",
                                  positive.class='GreyGibbons')


PerformanceOutPutTrained <- gibbonNetR::get_best_performance(performancetables.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/model_eval/performance_tables_trained/',
                                                             model.type = 'binary',class='Gibbons',Thresh.val =0.1)

PerformanceOutPutTrained$f1_plot
PerformanceOutPutTrained$best_f1$F1
PerformanceOutPutTrained$pr_plot
(PerformanceOutPutTrained$pr_plot)+scale_color_manual(values=matlab::jet.colors(6))

# Cambodia multi ----------------------------------------------------------

trained_models_dir <- "/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/model_output/top_models/combined_multi/"

#image_data_dir <- '/Volumes/DJC 1TB/VocalIndividualityClips/RandomSelectionImages/'
image_data_dir <- "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/Images/"

trainingfolder <- 'imagesmulti'

class_names <- c("CrestedGibbons", "GreyGibbons", "Noise")

evaluate_trainedmodel_performance_multi(trained_models_dir=trained_models_dir,
                                        class_names=class_names,
                                        trainingfolder=trainingfolder,
                                        image_data_dir=image_data_dir,
                                        output_dir="/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_multi/",
                                        noise.category = "Noise")



PerformanceOutPutTrained <- gibbonNetR::get_best_performance(performancetables.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/model_eval_multi/performance_tables_multi_trained',
                                                             model.type = 'multi',
                                                             class='CrestedGibbons',Thresh.val =0.1)

PerformanceOutPutTrained$f1_plot
PerformanceOutPutTrained$best_f1$F1
PerformanceOutPutTrained$pr_plot

(PerformanceOutPutTrained$pr_plot)+scale_color_manual(values=matlab::jet.colors(6))


trained_models_dir <- "/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/model_output/top_models/combined_multi/"

#image_data_dir <- '/Volumes/DJC 1TB/VocalIndividualityClips/RandomSelectionImages/'
image_data_dir <- "/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/Images/"

trainingfolder <- 'imagesmulti'

class_names <- c("CrestedGibbons", "GreyGibbons", "Noise")

evaluate_trainedmodel_performance_multi(trained_models_dir=trained_models_dir,
                                        class_names=class_names,
                                        trainingfolder=trainingfolder,
                                        image_data_dir=image_data_dir,
                                        output_dir="/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/model_eval_multi/",
                                        noise.category = "Noise")



PerformanceOutPutTrained <- gibbonNetR::get_best_performance(performancetables.dir='/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/DanumValley/model_eval_multi/performance_tables_multi_trained',
                                                             model.type = 'multi',
                                                             class='GreyGibbons',Thresh.val =0.1)

PerformanceOutPutTrained$f1_plot
PerformanceOutPutTrained$best_f1$F1
PerformanceOutPutTrained$pr_plot

# Evaluate performance on different site/species test data ----------------------------------
library(flextable)
library(dplyr)

# Crested binary
PerformanceOutputTestCrestedBinary <- gibbonNetR::get_best_performance(performancetables.dir='model_output/testdata_eval/cambodia_binary/performance_tables_trained/',
                                                                       model.type = 'binary',class='Gibbons',
                                                                       Thresh.val = 0.5)

PerformanceOutputTestCrestedBinary$f1_plot
PerformanceOutputTestCrestedBinary$best_f1$F1
PerformanceOutputTestCrestedBinary$best_precision

PerformanceOutputTestCrestedBinary$best_f1 <- PerformanceOutputTestCrestedBinary$best_f1[,-c(19)]

# Grey gibbons binary
PerformanceOutputTestGreyBinary <- gibbonNetR::get_best_performance(performancetables.dir='model_output/testdata_eval/malaysia_binary/performance_tables_trained/',
                                                                    model.type = 'binary',class='Gibbons',Thresh.val = 0.5)

PerformanceOutputTestGreyBinary$f1_plot
PerformanceOutputTestGreyBinary$best_f1$F1
PerformanceOutputTestGreyBinary$best_f1  <- PerformanceOutputTestGreyBinary$best_f1[,-c(19)]


performancetables.dir.multi <- 'model_output/_imagesmulti_multi_unfrozen_TRUE_/performance_tables_multi/'

PerformanceOutputTestMultiCrested <-  gibbonNetR::get_best_performance(performancetables.dir='model_output/testdata_eval/combined_multi/performance_tables_multi_trained',
                                                                       class='CrestedGibbons',Thresh.val = 0.5)

PerformanceOutputTestMultiCrested$f1_plot
PerformanceOutputTestMultiCrested$best_f1

PerformanceOutputTestMultiGrey <- gibbonNetR::get_best_performance(performancetables.dir='model_output/testdata_eval/combined_multi/performance_tables_multi_trained',
                                                                   class='GreyGibbons',Thresh.val = 0.5)

PerformanceOutputTestMultiGrey$f1_plot

PerformanceOutputTestMultiGrey$best_f1$F1

PerformanceOutputTestCrestedBinary$best_f1$Species <- 'Crested Gibbon'
PerformanceOutputTestGreyBinary$best_f1$Species <- 'Grey Gibbon'
PerformanceOutputTestMultiCrested$best_f1$Species <- 'Crested Gibbon'
PerformanceOutputTestMultiGrey$best_f1$Species  <- 'Grey Gibbon'

PerformanceOutputTestCrestedBinary$best_precision$Species <- 'Crested Gibbon'
PerformanceOutputTestGreyBinary$best_precision$Species <- 'Grey Gibbon'
PerformanceOutputTestMultiCrested$best_precision$Species <- 'Crested Gibbon'
PerformanceOutputTestMultiGrey$best_precision$Species  <- 'Grey Gibbon'

PerformanceOutputTestCrestedBinary$best_recall$Species <- 'Crested Gibbon'
PerformanceOutputTestGreyBinary$best_recall$Species <- 'Grey Gibbon'
PerformanceOutputTestMultiCrested$best_recall$Species <- 'Crested Gibbon'
PerformanceOutputTestMultiGrey$best_recall$Species  <- 'Grey Gibbon'

columns_sub <- c("Species", "Training Data", "N epochs", "CNN Architecture", "Threshold", "Precision", "Recall", "F1")

CombinedDFTest <-
  rbind.data.frame(PerformanceOutputTestCrestedBinary$best_f1[, columns_sub],
                   PerformanceOutputTestGreyBinary$best_f1[, columns_sub],
                   PerformanceOutputTestMultiCrested$best_f1[, columns_sub],
                   PerformanceOutputTestMultiGrey$best_f1[, columns_sub],
                   PerformanceOutputTestCrestedBinary$best_recall[, columns_sub],
                   PerformanceOutputTestGreyBinary$best_recall[, columns_sub],
                   PerformanceOutputTestMultiCrested$best_recall[, columns_sub],
                   PerformanceOutputTestMultiGrey$best_recall[, columns_sub],
                   PerformanceOutputTestCrestedBinary$best_precision[, columns_sub],
                   PerformanceOutputTestGreyBinary$best_precision[, columns_sub],
                   PerformanceOutputTestMultiCrested$best_precision[, columns_sub],
                   PerformanceOutputTestMultiGrey$best_precision[, columns_sub]
  )

nrow(CombinedDFTest)
head(CombinedDFTest)

CombinedDFTest$Precision <- round(CombinedDFTest$Precision,2)
CombinedDFTest$Recall <- round(CombinedDFTest$Recall,2)
CombinedDFTest$F1 <- round(CombinedDFTest$F1,2)

CombinedDFTest$`Training Data` <- ifelse(CombinedDFTest$`Training Data`=='imagesmulti','Multi-class','binary')
CombinedDFTest$`Training Data` <- paste(CombinedDFTest$Species, CombinedDFTest$`Training Data`)
CombinedDFTest <-CombinedDFTest[,-c(1)]

CombinedDFTest$`CNN Architecture` <-
  str_split_fixed(CombinedDFTest$`CNN Architecture`,pattern = '_',n=2)[,1]


CombinedDFTestFlextable <- flextable(CombinedDFTest)
CombinedDFTestFlextable



flextable::save_as_docx(CombinedDFTestFlextable,
                        path='Table 2 Performance on test data.docx')


# Create precision, recall, F1 curve for top models

CrestedTopBinary <- read.csv('model_output/testdata_eval/cambodia_binary/performance_tables_trained/imagescambodia_20_resnet50_model_TransferLearningTrainedModel.csv')

ggplot(data = CrestedTopBinary, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1"), linetype = "solid") +
  geom_line(aes(y = Precision, color = "Precision"), linetype = "solid") +
  geom_line(aes(y = Recall, color = "Recall"), linetype = "solid") +
  labs(title = "Crested Gibbons (binary)",
       x = "Thresholds",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  theme_minimal()+
  theme(legend.title = element_blank())# +xlim(0.5,1)

GreyTopBinary <- read.csv('model_output/testdata_eval/malaysia_binary/performance_tables_trained/imagesmalaysia_4_resnet152_model_TransferLearningTrainedModel.csv')

ggplot(data = GreyTopBinary, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1"), linetype = "solid") +
  geom_line(aes(y = Precision, color = "Precision"), linetype = "solid") +
  geom_line(aes(y = Recall, color = "Recall"), linetype = "solid") +
  labs(title = "Grey Gibbons (binary)",
       x = "Thresholds",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  theme_minimal()+
  theme(legend.title = element_blank())# +xlim(0.5,1)

GreyTopMulti <- read.csv('model_output/testdata_eval/combined_multi/performance_tables_multi_trained/imagesmulti_5_resnet50_model_TransferLearningTrainedModel.csv')

GreyTopMulti <- subset(GreyTopMulti,Class=='GreyGibbons')

ggplot(data = GreyTopMulti, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1"), linetype = "solid") +
  geom_line(aes(y = Precision, color = "Precision"), linetype = "solid") +
  geom_line(aes(y = Recall, color = "Recall"), linetype = "solid") +
  labs(title = "Grey Gibbons (multi)",
       x = "Thresholds",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  theme_minimal()+
  theme(legend.title = element_blank())# +xlim(0.5,1)

CrestedTopMulti <- read.csv('model_output/testdata_eval/combined_multi/performance_tables_multi_trained/imagesmulti_3_resnet50_model_TransferLearningTrainedModel.csv')

CrestedTopMulti <- subset(CrestedTopMulti,Class=='CrestedGibbons')

ggplot(data = CrestedTopMulti, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1"), linetype = "solid") +
  geom_line(aes(y = Precision, color = "Precision"), linetype = "solid") +
  geom_line(aes(y = Recall, color = "Recall"), linetype = "solid") +
  labs(title = "Crested Gibbons (multi)",
       x = "Thresholds",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  theme_minimal()+
  theme(legend.title = element_blank())# +xlim(0.5,1)


setwd("/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies")
devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")


CrestedTopBinary <- read.csv('model_output/testdata_eval/malaysia_binary/performance_tables_trained/imagesmalaysia_1_resnet50_model_TransferLearningTrainedModel.csv')

#CrestedTopBinary <- read.csv('/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/model_output/testdata_eval/cambodia_binary/performance_tables_trained/imagescambodia_1_alexnet_model_TransferLearningTrainedModel.csv')

ggplot(data = CrestedTopBinary, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1"), linetype = "solid") +
  geom_line(aes(y = Precision, color = "Precision"), linetype = "solid") +
  geom_line(aes(y = Recall, color = "Recall"), linetype = "solid") +
  labs(title = "Crested Gibbons (binary)",
       x = "Thresholds",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  theme_minimal()+
  theme(legend.title = element_blank())# +xlim(0.5,1)

