#Sean Steinle
#CS1675 Final Project
#Classification D:  Complex Models

#IMPORTS AND PATH
library(tidyverse)
library(caret)
library(neuralnet)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA AND MODELS
df <- readr::read_csv("data/final_project_train.csv", col_names = TRUE) #use the test set now

#SETTING EVAULATION METRICS AND RESAMPLING SCHEME
control <- trainControl(method = "repeatedcv", number = 5, repeats = 3) #five fold, three repeats
metric <- "Accuracy"

#LINEAR MODELS (4 TOTAL)

#first let's extract a baseline accuracy
event_num <- df %>%
  filter(outcome == 'event') %>%
  count()
naive_acc <- (1 - event_num/nrow(df))
naive_acc #If we guess "non event" every single time, our accuracy will be 81.2%. Baseline is very high because data imbalanced!

#formulas
df$outcomePredictable <- factor(ifelse(df$outcome == 'event', 1, 0))

contonly <- df %>%
  select(-rowid, -region, -customer, -outcome, -response, -outcomePredictable) %>%
  names()

F1 <- as.formula(outcomePredictable ~ . - outcome - rowid - response) #formerly known as M3
F2 <- as.formula(outcomePredictable ~ region + customer + (. - response - rowid - region - customer)^2)
F3 <- as.formula(paste("outcomePredictable ~ ", paste(contonly, collapse = "+"), sep = "")) #formerly known as M2
F4 <- as.formula(paste("outcomePredictable ~ region*(", paste(contonly, collapse = "+"), ")", sep = ""))#since M3 is already included, include M4

#train models
#for accuracy
M1 <- train(F1, method = "glm", metric = metric, trControl = control, data = df)
M2 <- train(F2, method = "glm", metric = metric, trControl = control, data = df) #over 95% accuracy! throws numerical warnings
M3 <- train(F3, method = "glm", metric = metric, trControl = control, data = df)
M4 <- train(F4, method = "glm", metric = metric, trControl = control, data = df) #worse than baseline, yikes.

#analyze results
linear_results <- resamples(list(all_feats = M1,
                                 all_feats_interact = M2,
                                 cont_feats = M3,
                                 region_cont_interact = M4))
#analyze results
linear_results_roc <- resamples(list(M1 = M1_roc,
                                 M2 = M2_roc,
                                 M3 = M3_roc,
                                 M4 = M4_roc))

summary(linear_results)
dotplot(linear_results, metric = "Accuracy")
summary(linear_results_roc)
dotplot(linear_results_roc, metric = "ROC")

#ADVANCED MODELS (X TOTAL)
#train models
M5 <- train(F1, method = "glmnet", metric = metric, trControl = control, data = df)
M6 <- train(F4, method = "glmnet", metric = metric, trControl = control, data = df) 
M7 <- train(F1, method = "nnet", metric = metric, trControl = control, data = df)
M8 <- train(F1, method = "rf", metric = metric, trControl = control, data = df)
M9 <- train(F1, method = "xgbTree", metric = metric, trControl = control, data = df) 

complex_results <- resamples(list(enet_all_feats = M5,
                                  enet_all_feat_interact = M6,
                                  nnet = M7,
                                  rf = M8,
                                  xgb = M9))

dotplot(complex_results, metric = "Accuracy")

#METHODS OF MY CHOICE
M10 <- train(F1, method = "dnn", metric = metric, trControl = control, data = df) #deep neural net
M11 <- train(F1, method = "simpls", metric = metric, trControl = control, data = df) #partial least squares

custom_results <- resamples(list(dnn = M10,
                                 pls = M11))

dotplot(custom_results, metric = "Accuracy")
dotplot(custom_results, metric = "ROC")

#FINAL ANALYSIS
all_results <- resamples(list(all_feats = M1,
                              all_feats_interact = M2,
                              cont_feats = M3,
                              region_cont_interact = M4,
                              enet_all_feats = M5,
                              enet_all_feats_interact = M6,
                              nnet = M7,
                              rf = M8,
                              xgb = M9,
                              dnn = M10,
                              pls = M11))

#train models for ROC as well
df_roc <- df
df_roc$outcomeYN <- factor(ifelse(df$outcome == 'event', 'yes', 'no'))
roc_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, summaryFunction = twoClassSummary, classProbs = TRUE)

F1_roc <- as.formula(outcomeYN ~ . - outcome - rowid - outcomePredictable) #formerly known as M3
F2_roc <- as.formula(outcomeYN ~ region + customer + (. - response - rowid - region - customer)^2)
F3_roc <- as.formula(paste("outcomeYN ~ ", paste(contonly, collapse = "+"), sep = "")) #formerly known as M2
F4_roc <- as.formula(paste("outcomeYN ~ region*(", paste(contonly, collapse = "+"), ")", sep = ""))#since M3 is already included, include M4

M1_roc <- train(F1_roc, method = "glm", metric = "ROC", trControl = roc_control, data = df_roc)
M2_roc <- train(F2_roc, method = "glm", metric = "ROC", trControl = roc_control, data = df_roc)
M3_roc <- train(F3_roc, method = "glm", metric = "ROC", trControl = roc_control, data = df_roc)
M4_roc <- train(F4_roc, method = "glm", metric = "ROC", trControl = roc_control, data = df_roc)
M5_roc <- train(F1_roc, method = "glmnet", metric = "ROC", trControl = roc_control, data = df_roc)
M6_roc <- train(F4_roc, method = "glmnet", metric = "ROC", trControl = roc_control, data = df_roc)
M7_roc <- train(F1_roc, method = "nnet", metric = "ROC", trControl = roc_control, data = df_roc)
M8_roc <- train(F1_roc, method = "rf", metric = "ROC", trControl = roc_control, data = df_roc)
M9_roc <- train(F1_roc, method = "xgbTree", metric = "ROC", trControl = roc_control, data = df_roc)
M10_roc <- train(F1_roc, method = "dnn", metric = "ROC", trControl = roc_control, data = df_roc)
M11_roc <- train(F1_roc, method = "simpls", metric = "ROC", trControl = roc_control, data = df_roc)

all_results_roc <- resamples(list(all_feats = M1_roc,
                                  all_feats_interact = M2_roc,
                                  cont_feats = M3_roc,
                                  region_cont_interact = M4_roc,
                                  enet_all_feats = M5_roc,
                                  enet_all_feats_interact = M6_roc,
                                  nnet = M7_roc,
                                  rf = M8_roc,
                                  xgb = M9_roc,
                                  dnn = M10_roc,
                                  pls = M11_roc))

jpeg(file = "plots/classification/model_accuracies.jpeg")
dotplot(all_results)
dev.off()
jpeg(file = "plots/classification/model_ROCs.jpeg")
dotplot(all_results_roc)
dev.off()

#save best model
M1 %>% readr::write_rds('models/glm_norm_class.rds')
M2 %>% readr::write_rds('models/glm_class.rds')
M5 %>% readr::write_rds('models/glmnet_class.rds')

#WRITE-UP:

# It seems that the best model overall is the generalized linear model that contains all continuous and categorical features with continuous interactions. It has more uncertainty than other models
# in regards to the ROC especially but also the accuracy. Its mean accuracy is the only one above 90%, with the others topping out around 84% (right around baseline). However, this models seems to be
# badly overfit. Its coefficient values are very large and there is a great amount of uncertainty in its evaluation metrics, both signs of overfitting. Interestingly, 
# all of the models perform very similarly with regards to sensitivity, whereas the aforementioned best model is almost twice as specific as any other model. Thus, we say that the simplest of
# the next tier of models is the best, which would be M1, the glm() with continuous and categorical linear additive features.
