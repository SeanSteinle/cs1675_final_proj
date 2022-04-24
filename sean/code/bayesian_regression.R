#Sean Steinle
#CS1675 Final Project
#Regression

#IMPORTS AND PATH
library(tidyverse)
library(caret)
library(rstanarm)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA
df <- readr::read_csv("data/final_project_train.csv", col_names = TRUE)
colnames(df)

#TRAIN BAYESIAN MODELS

#trim df
df_train <- df %>%
  select(-rowid, -outcome)

#first, specify models identified from simple linear regression (in retrospect, my formulas could have been more concise in my last script)
F9 <- as.formula(log(response) ~ (region + customer):(. - (region + customer)))
F4 <- as.formula(log(response) ~ (region)*(. - (region + customer)))

#these will take a while to train!
bM9 <- stan_lm(F9, df, prior = R2(location = 0.966)) #We'll specify the rsq prior as what we found from our non-Bayesian models!
bM4 <- stan_lm(F4, df, prior = R2(location = 0.727))

#summarize results--which model is best?
nbM9_sigma <- 0.302
nbM4_sigma <- 0.242
bM9
bM4

#visualize coefficient posterior summary for best model


#what's the posterior uncertainty in the noise for our bayesian model? how does the MLE we got from lm() in the last script relate to our posterior uncertainty on noise in a bayesian setting?
