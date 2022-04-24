#Sean Steinle
#CS1675 Final Project
#Regression: Best Models

#IMPORTS AND PATH
library(tidyverse)
library(caret)
library(rstanarm)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA AND MODELS
df <- readr::read_csv("data/final_project_train.csv", col_names = TRUE)
M9 <- readr::read_rds('models/nbM9.rds') #using frequentist models because I have more experience with lm() style
M4 <- readr::read_rds('models/nbM4.rds')

#make predictions
M9_ci <- data.frame(predict(M9, df, interval = 'confidence')) #get rank-deficient fit warning because of collinearity in predictors or more features than samples, not sure
M9_pi <- data.frame(predict(M9, df, interval = 'prediction'))
M9_df <- M9_ci %>% left_join(M9_pi, by = 'fit') #why am I getting 2 extra predictions?
M4_ci <- data.frame(predict(M4, df, interval = 'confidence'))
M4_pi <- data.frame(predict(M4, df, interval = 'prediction'))
M4_df <- M4_ci %>% left_join(M4_pi, by = 'fit')

#make predictions for M9
M9_df %>%
  ggplot(aes(x = seq(1,679,length.out = 679))) +
  geom_line(aes(y = upr.y), color = 'orange') +
  geom_line(aes(y = upr.x), color = 'grey') +
  geom_point(aes(y = fit), color = 'red') +
  geom_line(aes(y = lwr.x), color = 'grey') +
  geom_line(aes(y = lwr.y), color = 'orange') +
  ylab("Estimate, Confidence Interval, and Prediction Interval") + xlab("Observation ID") +
  ggtitle("Uncertainty Intervals for Model 9") +
  theme_minimal()
ggsave("plots/regression/nbM9_intervals.png")

#make predictions for M4
M4_df %>%
  ggplot(aes(x = seq(1,679,length.out = 679))) +
  geom_line(aes(y = upr.y), color = 'orange') +
  geom_line(aes(y = upr.x), color = 'grey') +
  geom_point(aes(y = fit), color = 'red') +
  geom_line(aes(y = lwr.x), color = 'grey') +
  geom_line(aes(y = lwr.y), color = 'orange') +
  ylab("Estimate, Confidence Interval, and Prediction Interval") + xlab("Observation ID") +
  ggtitle("Uncertainty Intervals for Model 4") +
  theme_minimal()
ggsave("plots/regression/nbM4_intervals.png")

#THESE GRAPHS ARENT FINALIZED!
#need to:
# 1. merge mod dfs with full df
# 2. facet some cool stuff to study preds against
# 3. figure out where we got 2 extra obs lol
