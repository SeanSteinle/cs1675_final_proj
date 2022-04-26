#Sean Steinle
#CS1675 Final Project
#Classification B: Bayesian Models

#IMPORTS AND PATH
library(tidyverse)
library(caret)
library(rstanarm)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA
df <- readr::read_csv("data/final_project_train.csv", col_names = TRUE)
colnames(df)

#TRAIN BAYESIAN MODELS

#make predictable outcome
df$outcomePredictable <- ifelse(df$outcome == 'event', 1, 0)

#first, specify models identified from simple classification
F2 <- as.formula(paste("outcomePredictable ~ ", paste(contonly, collapse = "+"), sep = ""))
F3 <- as.formula(paste("outcomePredictable ~ region + customer +", paste(contonly, collapse = "+"), sep = ""))

#these will take a while to train!
bM2 <- stan_glm(F2, df, family = binomial) #diffuse priors
bM3 <- stan_glm(F3, df, family = binomial) 

#summarize results--which model is best?
bM2
bM3
posterior_interval(bM2, prob = 0.95, pars = c("log-posterior"))
#visualize coefficient posterior summary for best model


#non-probabilistic
dotplot(bM3$coefficients)

#probabilistic
bM3_means <- bM3$stan_summary[,1]
bM3_sds <- bM3$stan_summary[,2]  
bM3_upper <- bM3_means + (2*bM3_sds)
bM3_lower <- bM3_means - (2*bM3_sds)

bM3_ci <- data.frame(bM3_means, bM3_lower, bM3_upper)
bM3_ci %>%
  ggplot(aes(seq(1, 46, length.out = 46), bM3_means)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = bM3_lower, ymax = bM3_upper)) +
  ylim(c(-5,5)) + #eliminates one huge coefficient (-2000 or something)
  ylab("Confidence Interval") + xlab("Coefficient Index") +
  ggtitle("Confidence Intervals for Model 3 Coefficients (Cropped)") +
  theme_minimal()
ggsave("plots/classification/bM3_coefs_ci.png")

#now try for M2
bM2_means <- bM2$stan_summary[,1]
bM2_sds <- bM2$stan_summary[,2]  
bM2_upper <- bM2_means + (2*bM2_sds)
bM2_lower <- bM2_means - (2*bM2_sds)

bM2_ci <- data.frame(bM2_means, bM2_lower, bM2_upper)
bM2_ci %>%
  ggplot(aes(seq(1, 36, length.out = 36), bM2_means)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = bM2_lower, ymax = bM2_upper)) +
  ylim(c(-5,5)) + #eliminates one huge coefficient (-200 or something)
  ylab("Confidence Interval") + xlab("Coefficient Index") +
  ggtitle("Confidence Intervals for Model 2 Coefficients (Cropped)") +
  theme_minimal()
ggsave("plots/classification/bM2_coefs_ci.png")


#save models
bM3 %>% readr::write_rds('models/bM3_class.rds')
bM2 %>% readr::write_rds('models/bM2_class.rds')

#WRITE UP:
# Model 3 was the best performing model according to AIC of the models in the prior script, and I chose Model 2 because it was the only model with a similar AIC score that didn't have fitting warnings.
# In this case, both models looked very similar which is to be expected (the only difference between the models is that M3 has two extra inputs). Furthermore, both models had one coefficient that was extremely
# large, and I thus had to crop the coefficient plot. In either case, because the coefficients are very similar, M3 seems to be a better model because it had better AIC performance.