#Sean Steinle
#CS1675 Final Project
#Classification D: Complex Models

#IMPORTS AND PATH
library(tidyverse)
library(caret)
library(neuralnet)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA AND MODELS
df <- readr::read_csv("data/final_project_train.csv", col_names = TRUE) #use the test set now

#SETTING EVAULATION METRICS AND RESAMPLING SCHEME
control <- trainControl(method = "repeatedcv", number = 5, repeats = 3) #five fold, three repeats
metric <- "RMSE"

#LINEAR MODELS (4 TOTAL)

#formulas
contonly <- df %>%
  select(-rowid, -region, -customer, -outcome, -response) %>%
  names()

F1 <- as.formula(log(response) ~ . - outcome - rowid) #all feats
F2 <- as.formula(log(response) ~  region + customer + (. - outcome - rowid - region - customer)^2) #all feat interact
F3 <- as.formula(paste('log(response) ~ (region + customer):(',paste('poly(', contonly,',2)',collapse = ' + '), ")")) #formerly known as M9 (region + cust interact w cont)
F4 <- as.formula(paste("log(response) ~ region*(", paste(contonly, collapse = "+"), ")", sep = "")) #formerly known as M4 (region interact w cont)

#train models
M1 <- train(F1, method = "lm", metric = metric, trControl = control, preprocess = c('center', 'scale'), data = df)
M2 <- train(F2, method = "lm", metric = metric, trControl = control, preprocess = c('center', 'scale'), data = df)
M3 <- train(F3, method = "lm", metric = metric, trControl = control, preprocess = c('center', 'scale'), data = df)
M4 <- train(F4, method = "lm", metric = metric, trControl = control, preprocess = c('center', 'scale'), data = df)

#analyze results
linear_results <- resamples(list(M1 = M1,
                                 M2 = M2,
                                 M3 = M3,
                                 M4 = M4))

dotplot(linear_results, metric = "RMSE")
dotplot(linear_results, metric = "Rsquared")

#ADVANCED MODELS (X TOTAL)

#elasticnet regression X

#train models
M5 <- train(F1, method = "enet", metric = metric, trControl = control, preprocess = c('center', 'scale'), data = df)
M6 <- train(F4, method = "enet", metric = metric, trControl = control, preprocess = c('center', 'scale'), data = df) #I used F4 instead of F3 because F3 was taking an extreme amount of time to train
M7 <- train(F1, method = "nnet", metric = metric, trControl = control, preprocess = c('center', 'scale'), data = df)
M8 <- train(F1, method = "rf", metric = metric, trControl = control, preprocess = c('center', 'scale'), data = df) #this takes a while to train
M9 <- train(F1, method = "xgbTree", metric = metric, trControl = control, preprocess = c('center', 'scale'), data = df) #this too

#neural net 
#random forest (rf) 
#gradient boosted tree (xgbTree)

complex_results <- resamples(list(M5 = M5,
                                  M6 = M6,
                                  M7 = M7,
                                  M8 = M8,
                                  M9 = M9))

dotplot(complex_results, metric = "RMSE")
dotplot(complex_results, metric = "Rsquared")

#METHODS OF MY CHOICE
M10 <- train(F1, method = "dnn", metric = metric, trControl = control, data = df) #deep neural net
M11 <- train(F1, method = "simpls", metric = metric, trControl = control, data = df) #partial least squares

custom_results <- resamples(list(M10 = M10,
                                 M11 = M11))

#FINAL ANALYSIS
all_results_cropped <- resamples(list(all_feats = M1,
                              interact_region_cont = M4,
                              enet_all_feats = M5,
                              enet_all_feats_interact_cont = M6,
                              nnet = M7,
                              rf = M8,
                              xgb = M9,
                              dnn = M10,
                              pls = M11))

all_results <- resamples(list(all_feats = M1,
                                      all_feats_interact_cont = M2, #These models are so poor they mess the scale up.
                                      interact_cat_cont = M3,
                                      interact_region_cont = M4,
                                      enet_all_feats = M5,
                                      enet_all_feats_interact_cont = M6,
                                      nnet = M7,
                                      rf = M8,
                                      xgb = M9,
                                      dnn = M10,
                                      pls = M11))

#save plots
jpeg(file = "plots/regression/model_regress_perform.jpeg")
dotplot(all_results_cropped)
dev.off()
jpeg(file = "plots/regression/model_regress_perform_full.jpeg")
dotplot(all_results)
dev.off()

#save best model
M1 %>% readr::write_rds('models/best_regress.rds')

#WRITE-UP:
# From the performance metrics gathered (Rsquared, MAE, RMSE), it looks like there is not so much a best model as much as there is a tier of best models. This tier consists of XGBoost,
# random forest, elasticnet (categorical and continuous features), linear model (categorical and continous features), and the linear model that interacts region with the continuous features.
# Out of these models, I think the linear model with categorical and continous features is the simplest and thus I would state that it is the best model. That said, XGBoost does have a slight edge in performance.
