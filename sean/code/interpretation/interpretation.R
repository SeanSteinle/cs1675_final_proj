#Sean Steinle
#CS1675 Final Project
#Interpretation

#IMPORTS AND PATH
library(tidyverse)
library(caret)
library(coefplot)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA AND MODELS
df <- readr::read_csv("data/final_project_train.csv", col_names = TRUE) #use the test set now
test_df <- readr::read_csv("data/final_project_holdout_inputs.csv", col_names = TRUE) #use the test set now
classGLM_overfit <- readr::read_rds('models/glm_class.rds')
classGLM_norm <- readr::read_rds('models/glm_norm_class.rds')
classGLMNET <- readr::read_rds('models/glmnet_class.rds')
regressModel <- readr::read_rds('models/best_regress.rds')


#PART i) BEST MODELS AND INFLUENTIAL INPUTS!

#finding important inputs
coefplot(classGLM_overfit) #HORRIBLY large coefficients.
jpeg(file = "plots/interpretation/class_model_coefficients.jpeg")
coefplot(classGLM_norm) #same performance as elasticnet, but we can quantify uncertainty here!
dev.off()
coefplot(classGLMNET$finalModel)
jpeg(file = "plots/interpretation/regress_model_coeffients.jpeg")
coefplot(regressModel)
dev.off()

#WRITE-UP:
# THE BEST MODEL:
# As mentioned in the previous section, the best models for regression is the linear model with categorical and continuous inputs. This model is very simple yet performs
# nearly as well as all other models in regression. Classification is a bit trickier. Originally, I was convinced that the generalized linear model with categorical and continuous
# inputs with continuous interactions was the best model, but upon visualizing the coefficients of the model, it seems like the model is very badly overfit--the coefficients are extremely
# large values. Additionally, this model has a very wide confidence interval for its accuracy and ROC which could also indicate overfitting. If I had trained many more folds during
# cross-validation, I may have seen this earlier. Because of these findings, I am going to need to select a model from the next "tier" of models, which all perform fairly similarly.
# I am going to choose the simple generalized linear model with linear additive categorical and continuous features as my best classification model because it performs almost exactly as well as
# the other models but it also allows us to quantify uncertainty on its coefficients.

# THE MOST IMPORTANT FEATURES:
# Conveniently for analysis, our best models did not use any interaction terms and they shared a very similar set of features. In terms of the categorical features, they did prove to 
# be somewhat useful predictors in both tasks. For regression, the region attribute was more useful than the customer attribute because while only 4/8 customers were significant predictors,
# 2/3 regions were significant predictors. For the classification task things swapped: customers were a better predictor with 5/8 features being significant whereas only 1/3 region features
# were significant. 
# In terms of the continuous (sentiment) features, there proved to be useful features (NRC, Bing, AFCINN) and useless features (sentimentr, word-based). The NRC, Bing, 
# and AFCINN features were all very similar and all had some of their features prove significant for regression or classification. Specifically, Bing features were effective at regression
# whereas AFCINN and NRC features were more flexible and able to significantly contribute to prediction in both tasks. Interestingly, the uncertainty structure of these features in the
# classification task was such that lower numbered features (1-4) has small uncertainty, whereas higher numbered features (5-8) had larger uncertainty. This was not the case with regression,
# as all feature numbers had similar variances. Both word-based features and sentimentr features were fairly useless. The word-based features had extremely small uncertainty but also
# very small beta coefficients, so they had little impact. The sentimentr features were quite horrible at prediction as they had large uncertainties and none of the features proved significant
# in either task.
# Finally, I would say that a subset of the sentiment features (A, B, and N) were useful in both tasks, but that only the regression task was a success.
# Predicting hours worked with a >60% r-squared value is a useful model, but predicting successful sales targets at just a few percentage points above the baseline
# of 81% is not very useful at all.


#PART ii) #PREDICTING THE HOLDOUT SET

#retraining my models (due to caret error that didn't like lack of comfortity between train df and test df!)
F1 <- as.formula(log(response) ~ .)
train_df <- df %>% select(-outcome, -rowid)
M1 <- train(F1, method = "lm", metric = metric, trControl = control, preprocess = c('center', 'scale'), data = train_df)
regress_preds <- predict(M1, test_df)

F2 <- as.formula(outcomePredictable ~ .) #formerly known as M3
train_df <- df %>%
  mutate(outcomePredictable = factor(ifelse(df$outcome == 'event', 1, 0))) %>%
  select(-outcome, -response, -rowid)
M2 <- train(F2, method = "glm", metric = "Accuracy", trControl = control, data = train_df)
class_preds <- predict(M2, newdata = test_df, type = 'prob')[2] #probs of event
class_preds

myPreds <- data.frame(regress_preds, class_preds)
myPreds$outcome <- factor(ifelse(myPreds$X1 >= 0.5, 'event', 'non-event'))
myPreds <- myPreds %>%
  relocate(X1, .after = outcome) %>%
  tibble::rowid_to_column()
names(myPreds) <- c('id', 'y', 'outcome', 'probability')
myPreds %>% readr::write_csv('data/myPreds.csv')

#PART iii) VISUALIZING TRENDS
#need to make rowid a key we can left join on
names(myPreds) <- c('rowid', 'pred_response', 'pred_outcome', 'pred_probability')

test_df <- test_df %>%
  select(-rowid) %>%
  tibble::rowid_to_column()

fullPreds <- myPreds %>%
  left_join(test_df, by = 'rowid')

fullPreds %>% glimpse()

#now we can draw predictive trends!

fullPreds %>% #run chart
  ggplot(aes(x = seq(1,nrow(fullPreds),length.out = nrow(fullPreds)))) +
  geom_line(aes(y = pred_response), color = 'black') +
  geom_point(aes(y = pred_response), color = 'red') +
  ylab("Estimates of Log Hours Worked") + xlab("Observation ID") +
  ggtitle("Run Chart of Best Regression Model Predictions") +
  theme_minimal()
ggsave("plots/interpretation/regress_run.png")

fullPreds %>% #plot by customers and sent feature
  ggplot(aes(x = xb_04)) +
  geom_point(aes(y = pred_response), color = 'red') +
  ylab("Estimates of Log Hours Worked") + xlab("Bing IV Value") +
  ggtitle("Best Regression Model Predictions per Customer by Bing IV Feature Value") +
  facet_wrap(~customer)
  theme_minimal()
ggsave("plots/interpretation/regress_b4_cust.png")





