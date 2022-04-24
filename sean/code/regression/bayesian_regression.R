#Sean Steinle
#CS1675 Final Project
#Regression: Bayesian Models

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
posterior_interval(bM9, prob = 0.95, pars = c("R2","sigma"))
posterior_interval(bM4, prob = 0.95, pars = c("R2","sigma"))

#visualize coefficient posterior summary for best model

#non-probabilistic
dotplot(bM9$coefficients)


#probabilistic
bM9_means <- bM9$stan_summary[,1]
bM9_sds <- bM9$stan_summary[,2]  
bM9_upper <- bM9_means + (2*bM9_sds)
bM9_lower <- bM9_means - (2*bM9_sds)

bM9_ci <- data.frame(bM9_means, bM9_lower, bM9_upper)
bM9_ci %>%
  ggplot(aes(seq(1, 384, length.out = 384), bM9_means)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = bM9_lower, ymax = bM9_upper)) +
  ylim(c(-2.5,2.5)) +
  ylab("Confidence Interval") + xlab("Coefficient Index") +
  ggtitle("Confidence Intervals for Model 9 Coefficients (Cropped)") +
  theme_minimal()
ggsave("plots/regression/bM9_coefs_ci.png")

#save models
bM9 %>% readr::write_rds('models/bM9.rds')
bM4 %>% readr::write_rds('models/bM4.rds')

#WRITE UP:
# We are quite certain as to the level of noise in our Bayesian setting because rstanarm gives us a scaling of the Median Absolute Deviation (MAD) which can
# be used to construct a Bayesian uncertainty interval (compare to a Frequentist confidence interval). We find that for both models, the interval is quite small 
# (less than 0.04 in size for each). Furthermore, we find that our point estimates of sigma from the non-Bayesian lm() training we performed in the previous script
# are both near the middle of our uncertainty intervals.