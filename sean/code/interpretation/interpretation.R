#Sean Steinle
#CS1675 Final Project
#Interpretation

#IMPORTS AND PATH
library(tidyverse)
library(caret)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA AND MODELS
df <- readr::read_csv("data/final_project_train.csv", col_names = TRUE) #use the test set now
classModel <- readr::read_rds('models/best_class.rds')