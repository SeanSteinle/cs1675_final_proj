#Sean Steinle
#CS1675 Final Project
#Regression

#IMPORTS AND PATH
library(tidyverse)
library(caret)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA
df <- readr::read_csv("data/final_project_train.csv", col_names = TRUE)
colnames(df)

df %>% glimpse()

#REGRESSION WITH SIMPLE PROBLEMS
#This function fits num_pcs number of models and makes predictions for each PC.
fit_and_predict <- function(num_pcs, feat_names, df)
{ 
  models = list()
  preds = list()
  for(pc_num in 1:num_pcs)
  {
    fmla <- as.formula(paste("PC", pc_num, " ~ ", paste(feat_names, collapse= "+"), sep = ""))
    model <- lm(fmla, data = df)
    X <- df %>%
      select(contains("avg"))
    preds[[pc_num]] <- predict(model, X)
    models[[pc_num]] <- model
  }
  list("models" = models, "predictions" = preds)
}

#create feats and train models!
contonly <- df %>%
  select(-rowid, -region, -customer, -outcome, -response) %>%
  names()

F7 <- as.formula(paste('log(response) ~ ',paste('poly(', contonly,',2)',collapse = ' + ')))
F8 <- as.formula(paste('log(response) ~ region + customer +',paste('poly(', contonly,',2)',collapse = ' + ')))
F9 <- as.formula(paste('log(response) ~ (region + customer):(',paste('poly(', contonly,',2)',collapse = ' + '), ")"))

M1 <- lm(response ~ region + customer, data = df)
M2 <- lm(as.formula(paste("log(response) ~ ", paste(contonly, collapse = "+"), sep = "")), data = df) #a little convoluted, but fits easier than typing everything or doing -
M3 <- lm(as.formula(paste("log(response) ~ region + customer +", paste(contonly, collapse = "+"), sep = "")), data = df)
M4 <- lm(as.formula(paste("log(response) ~ region*(", paste(contonly, collapse = "+"), ")", sep = "")), data = df)
M5 <- lm(as.formula(paste("log(response) ~ customer*(", paste(contonly, collapse = "+"), ")", sep = "")), data = df)
M6 <- lm(as.formula(paste("log(response) ~ (", paste(contonly, collapse = "+"), ")^2", sep = "")), data = df) #len 562, pairwise interaction of 33 inputs
M7 <- lm(F7, data = df) #len 67. 33 cont inputs, 2 feats per input
M8 <- lm(F8, data = df) # #len 77. 33 cont inputs, 2 cat inputs, 2 feats per input
M9 <- lm(F9, data = df) #len 727. 33 cont inputs interacted w 2 cat inputs, 2 feats per input

#analyze adjusted r-squared!
summary(M1)$adj.r.squared
summary(M2)$adj.r.squared
summary(M3)$adj.r.squared
summary(M4)$adj.r.squared
summary(M5)$adj.r.squared
summary(M6)$adj.r.squared
summary(M7)$adj.r.squared
summary(M8)$adj.r.squared
summary(M9)$adj.r.squared #80%! not bad!

#top 3 model coef summaries
jpeg(file = "plots/regression/m9_coefs.jpeg")
dotplot(coef(M9))
dev.off()
jpeg(file = "plots/regression/m4_coefs.jpeg")
dotplot(coef(M4))
dev.off()
jpeg(file = "plots/regression/m8_coefs.jpeg")
dotplot(coef(M8))
dev.off()

#see largest coefs
sort(M9$coefficients)[-1]
sort(M4$coefficients)[-1]
sort(M8$coefficients)[-1]

#save rsq, sigma on M9 and M4 for reference during Bayesian modeling
m9_rsq <- summary(M9)$r.squared
m4_rsq <- summary(M4)$r.squared
m9_sigma <- summary(M9)$sigma
m4_sigma <- summary(M4)$sigma

#WRITE-UP:

#I used adjusted r-squared as my metric because I like the interpretability of r-squared, but I also wanted to penalize for model complexity because a few of my models have
#an extreme number of features.

#Out of my three best models, I was most impressed by model 4 (region interacted with sentiment features) because non of its coefficients exceeded an absolute value of 1
#yet it still was within 13% of the best model's performance. Conversely, model 9, which have dozens and hundreds of features respectively, had extremely large coefficient
#values for some features which is somewhat worriesome because it may indicate overfitting. Model 8 had large coefficients (>|5|) but not nearly the same scale as M9.
#The most important features for M9 seemed to involve interactions with customers #D and K, which may indicate that their sentiment features are especially predictive in some way. 
#M8's most influential features tended to be the inputs themselves (1st degree polys). M4's most influential features included region interactions with sentiment features, along with some
#non-interactive terms as well.

#MOST IMPOTANT FEATURES? 