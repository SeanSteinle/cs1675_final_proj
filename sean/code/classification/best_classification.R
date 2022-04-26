#Sean Steinle
#CS1675 Final Project
#Classification C: Best Models

#IMPORTS AND PATH
library(tidyverse)
library(caret)
library(confint)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA AND MODELS
df <- readr::read_csv("data/final_project_holdout_inputs.csv", col_names = TRUE) #use the test set now
M3 <- readr::read_rds('models/nbM3_class.rds') #using frequentist models because I have more experience with lm() style
M2 <- readr::read_rds('models/nbM2_class.rds')

#make predictions (with confidence interval, somewhat tedious)
preds <- predict(M3, df, type = 'link', se.fit = TRUE)
upper_bound <- M3$family$linkinv(preds$fit + (1.96 * preds$se.fit))
lower_bound <- M3$family$linkinv(preds$fit - (1.96 * preds$se.fit))
means <- M3$family$linkinv(preds$fit)

M3_df <- data.frame(means, lower_bound, upper_bound) %>%
  tibble::rowid_to_column() %>%
  left_join(df %>%
              select(-rowid) %>%
              tibble::rowid_to_column(), by = 'rowid')

#now for m2
preds <- predict(M2, df, type = 'link', se.fit = TRUE)
upper_bound <- M2$family$linkinv(preds$fit + (1.96 * preds$se.fit))
lower_bound <- M2$family$linkinv(preds$fit - (1.96 * preds$se.fit))
means <- M2$family$linkinv(preds$fit)

M2_df <- data.frame(means, lower_bound, upper_bound) %>%
  tibble::rowid_to_column() %>%
  left_join(df %>%
              select(-rowid) %>%
              tibble::rowid_to_column(), by = 'rowid')

#PREDICTING WITH M3:
M3_df %>% #non cropped run chart
  ggplot(aes(x = seq(1,nrow(M3_df),length.out = nrow(M3_df)))) +
  geom_line(aes(y = upper_bound), color = 'grey') +
  geom_point(aes(y = means), color = 'red') +
  geom_line(aes(y = lower_bound), color = 'grey') +
  ylab("Estimates of Sales Target Success Probability") + xlab("Observation ID") +
  ggtitle("Run Chart of Model 3 Predictions") +
  theme_minimal()
ggsave("plots/classification/nbM3_run.png")

M3_df %>% #(IMPORTANT INPUTS: N04, S04)
  ggplot(aes(x = xn_04, length.out = nrow(M3_df))) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), color = 'grey') +
  geom_point(aes(y = means), color = 'red') +
  facet_wrap(~(xs_04 > median(xs_04))) +
  ylab("Estimates of Sales Target Success Probability") + xlab("NRC Feature IV (XS_04)") +
  ggtitle("Model 3 Predictions by NRC Feature IV and sentimentr Feature IV") +
  theme_minimal()
ggsave("plots/classification/nbM3_n4.png")


#PREDICTING WITH M2:
M2_df %>% #non cropped run chart
  ggplot(aes(x = seq(1,nrow(M2_df),length.out = nrow(M2_df)))) +
  geom_line(aes(y = upper_bound), color = 'grey') +
  geom_point(aes(y = means), color = 'red') +
  geom_line(aes(y = lower_bound), color = 'grey') +
  ylab("Estimates of Sales Target Success Probability") + xlab("Observation ID") +
  ggtitle("Run Chart of Model 2 Predictions") +
  theme_minimal()
ggsave("plots/classification/nbM2_run.png")

M2_df %>% #(IMPORTANT INPUTS: N04, S04)
  ggplot(aes(x = xn_04, length.out = nrow(M2_df))) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), color = 'grey') +
  geom_point(aes(y = means), color = 'red') +
  facet_wrap(~(xs_04 > median(xs_04))) +
  ylab("Estimates of Sales Target Success Probability") + xlab("NRC Feature IV (XS_04)") +
  ggtitle("Model 2 Predictions by NRC Feature IV and sentimentr Feature IV") +
  theme_minimal()
ggsave("plots/classification/nbM2_n4.png")

# WRITE-UP:
# The trends between the two models are very similar which we might expect given their very similar formulas. Notably, we also find that the confidence intervals for both models
# are very wide/vague, so we are not very certain on our MLE prediction of mu.  It's also somewhat difficult to find features that have a relation with the outcome because the dataset
# is very imbalanced (is it just a few noisy points or is there really a correlation?) but it seems like sentimentr's feature 4 seems to do a fairly good job of distinguishing likely sales
# successes vs. failures (it is negatively correlated with sale success).

#are the trends consistent? yes!

#notes: conf intervals very vague, imabalanced dataset