#Sean Steinle
#CS1675 Final Project
#Regression C: Best Models

#IMPORTS AND PATH
library(tidyverse)
library(caret)
library(rstanarm)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA AND MODELS
df <- readr::read_csv("data/final_project_holdout_inputs.csv", col_names = TRUE) #use the test set now
M9 <- readr::read_rds('models/nbM9.rds') #using frequentist models because I have more experience with lm() style
M4 <- readr::read_rds('models/nbM4.rds')

#make predictions
M9_ci <- data.frame(predict(M9, df, interval = 'confidence')) #get rank-deficient fit warning because of collinearity in predictors or more features than samples, not sure
M9_pi <- data.frame(predict(M9, df, interval = 'prediction'))
M9_df <- M9_ci %>% left_join(M9_pi, by = 'fit') #why am I getting 2 extra predictions?
M4_ci <- data.frame(predict(M4, df, interval = 'confidence'))
M4_pi <- data.frame(predict(M4, df, interval = 'prediction'))
M4_df <- M4_ci %>% left_join(M4_pi, by = 'fit')

M9_df <- M9_df %>%
  tibble::rowid_to_column() %>%
  left_join(df %>%
              select(-rowid) %>%
              tibble::rowid_to_column(), by = 'rowid')

M4_df <- M4_df %>%
  tibble::rowid_to_column() %>%
  left_join(df %>%
              select(-rowid) %>%
              tibble::rowid_to_column(), by = 'rowid')


#PREDICTING WITH M9:
M9_df %>% #non cropped run chart
  ggplot(aes(x = seq(1,nrow(M9_df),length.out = nrow(M9_df)))) +
  geom_line(aes(y = upr.y), color = 'orange') +
  geom_line(aes(y = upr.x), color = 'grey') +
  geom_point(aes(y = fit), color = 'red') +
  geom_line(aes(y = lwr.x), color = 'grey') +
  geom_line(aes(y = lwr.y), color = 'orange') +
  ylab("Estimate and Intervals of Log Hours Worked") + xlab("Observation ID") +
  ggtitle("Run Chart of Model 9 Predictions (Uncropped)") +
  theme_minimal()
ggsave("plots/regression/nbM9_run_nocrop.png")

M9_df %>% #cropped run chart
  ggplot(aes(x = seq(1,nrow(M9_df),length.out = nrow(M9_df)))) +
  geom_line(aes(y = upr.y), color = 'orange') +
  geom_line(aes(y = upr.x), color = 'grey') +
  geom_point(aes(y = fit), color = 'red') +
  geom_line(aes(y = lwr.x), color = 'grey') +
  geom_line(aes(y = lwr.y), color = 'orange') +
  ylim(c(-10,10)) +
  ylab("Estimate and Intervals of Log Hours Worked") + xlab("Observation ID") +
  ggtitle("Run Chart of Model 9 Predictions (Cropped)") +
  theme_minimal()
ggsave("plots/regression/nbM9_run_crop.png")

M9_df %>% #uncropped pred chart by b4 and customer
  ggplot(aes(x = xb_04,length.out = nrow(M9_df))) +
  geom_point(aes(y = upr.y), color = 'orange') +
  geom_point(aes(y = upr.x), color = 'grey') +
  geom_point(aes(y = fit), color = 'red') +
  geom_point(aes(y = lwr.x), color = 'grey') +
  geom_point(aes(y = lwr.y), color = 'orange') +
  facet_wrap(~customer) +
  ylab("Estimate and Intervals of Log Hours Worked") + xlab("Bing Feature IV (XB_04)") +
  ggtitle("Model 9 Predictions by Bing Feature IV (Uncropped)") +
  theme_minimal()
ggsave("plots/regression/nbM9_b4_nocrop.png")

M9_df %>% #cropped pred chart by b4 and customer
  ggplot(aes(x = xb_04,length.out = nrow(M9_df))) +
  geom_errorbar(aes(ymin = lwr.y, ymax = upr.y), color = 'orange') +
  geom_errorbar(aes(ymin = lwr.x, ymax = upr.x), color = 'grey') +
  geom_point(aes(y = fit), color = 'red') +
  facet_wrap(~customer) +
  ylim(c(-10,10)) +
  ylab("Estimate and Intervals of Log Hours Worked") + xlab("Bing Feature IV (XB_04)") +
  ggtitle("Model 9 Predictions by Bing Feature IV (Cropped)") +
  theme_minimal()
ggsave("plots/regression/nbM9_b4_crop.png")


#PREDICTING WITH M4:
M4_df %>% #non cropped run chart
  ggplot(aes(x = seq(1,nrow(M4_df),length.out = nrow(M4_df)))) +
  geom_line(aes(y = upr.y), color = 'orange') +
  geom_line(aes(y = upr.x), color = 'grey') +
  geom_point(aes(y = fit), color = 'red') +
  geom_line(aes(y = lwr.x), color = 'grey') +
  geom_line(aes(y = lwr.y), color = 'orange') +
  ylab("Estimate and Intervals of Log Hours Worked") + xlab("Observation ID") +
  ggtitle("Run Chart of Model 4 Predictions (Uncropped)") +
  theme_minimal()
ggsave("plots/regression/nbM4_run.png")

M4_df %>% #uncropped pred chart by b4 and customer
  ggplot(aes(x = xs_04,length.out = nrow(M4_df))) +
  geom_errorbar(aes(ymin = lwr.y, ymax = upr.y), color = 'orange') +
  geom_errorbar(aes(ymin = lwr.x, ymax = upr.x), color = 'grey') +
  geom_point(aes(y = fit), color = 'red') +
  facet_wrap(~region) +
  ylab("Estimate and Intervals of Log Hours Worked") + xlab("sentimentr Feature IV (XS_04)") +
  ggtitle("Model 4 Predictions by sentimentr Feature IV (Uncropped)") +
  theme_minimal()
ggsave("plots/regression/nbM4_s4.png")

# WRITE-UP:
# Model 9 has been exposed! As we suspected from its extremely large coefficients, M9 is very badly overfit! It has some estimates of log(response) in the hundreds, which
# is far out of the range of reasonable response values. Even after cropping the plot to look at less extreme predictions, our confidence interval and prediction interval
# essentially overlap, another indication of overfitting. Finally, when we plot by values of Bing IV we see how the extreme coefficients of M9 drive the extreme predictions:
# there are several customers who have no predicted values between (-10,10).

# On the other hand, Model 4 performs fairly well! There are no predicted values outside of (-5,5), and there is a noticeable difference between the confidence interval and the prediction interval.
# We also see why sentimentr IV is an informative feature: large values of s4 are correlated with large response values for XX and YY, but not ZZ. Interestingly, these are our 3
# most informative features for the model! Simply put, M4 has worse training performance than M9, but M9 is hopelessly overfit whihle M4 generalizes well to the test set.