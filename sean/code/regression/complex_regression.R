#Sean Steinle
#CS1675 Final Project
#Regression D: Complex Models

#IMPORTS AND PATH
library(tidyverse)
library(caret)
library(rstanarm)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA AND MODELS
df <- readr::read_csv("data/final_project_holdout_inputs.csv", col_names = TRUE) #use the test set now
M9 <- readr::read_rds('models/nbM9.rds') #using frequentist models because I have more experience with lm() style
M4 <- readr::read_rds('models/nbM4.rds')