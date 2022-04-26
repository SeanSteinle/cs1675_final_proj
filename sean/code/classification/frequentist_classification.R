#Sean Steinle
#CS1675 Final Project
#Classification A: Simple Frequentist Models

#IMPORTS AND PATH
library(tidyverse)
library(caret)
library(coefplot)
setwd("C:/Users/seans/Desktop/School/School/Sem8/CS1675/cs1675_final_proj/sean")

#LOADING DATA
df <- readr::read_csv("data/final_project_train.csv", col_names = TRUE)
colnames(df)

df %>% glimpse()

#CLASSIFICATION WITH SIMPLE PROBLEMS

#create predictable outcome
df$outcomePredictable <- ifelse(df$outcome == 'event', 1, 0)

#create feats and train models!
contonly <- df %>%
  select(-rowid, -region, -customer, -outcome, -response, -outcomePredictable) %>%
  names()

contonly

F7 <- as.formula(paste('outcomePredictable ~ ', paste('poly(', contonly,',2)',collapse = ' + ')))
F8 <- as.formula(paste('outcomePredictable ~ region + customer +',paste('poly(', contonly,',2)',collapse = ' + ')))
F9 <- as.formula(paste('outcomePredictable ~ (region + customer):(',paste('poly(', contonly,',2)',collapse = ' + '), ")"))

M1 <- glm(outcomePredictable ~ region + customer, data = df, family = binomial)
M2 <- glm(as.formula(paste("outcomePredictable ~ ", paste(contonly, collapse = "+"), sep = "")), data = df, family = binomial) #a little convoluted, but fits easier than typing everything or doing -
M3 <- glm(as.formula(paste("outcomePredictable ~ region + customer +", paste(contonly, collapse = "+"), sep = "")), data = df, family = binomial)
M4 <- glm(as.formula(paste("outcomePredictable ~ region*(", paste(contonly, collapse = "+"), ")", sep = "")), data = df, family = binomial)
M5 <- glm(as.formula(paste("outcomePredictable ~ customer*(", paste(contonly, collapse = "+"), ")", sep = "")), data = df, family = binomial)
M6 <- glm(as.formula(paste("outcomePredictable ~ (", paste(contonly, collapse = "+"), ")^2", sep = "")), data = df, family = binomial) #len 562, pairwise interaction of 33 inputs
M7 <- glm(F7, data = df, family = binomial) #len 67. 33 cont inputs, 2 feats per input
M8 <- glm(F8, data = df, family = binomial) # #len 77. 33 cont inputs, 2 cat inputs, 2 feats per input
M9 <- glm(F9, data = df, family = binomial) #len 727. 33 cont inputs interacted w 2 cat inputs, 2 feats per input

#analyze adjusted r-squared!
aics <- data.frame(c(
summary(M1)$aic,
summary(M2)$aic, #close 2nd
summary(M3)$aic, #best performer!
summary(M4)$aic,
summary(M5)$aic,
summary(M6)$aic,
summary(M7)$aic,
summary(M8)$aic,
summary(M9)$aic))#distant 3rd
names(aics) <- 'aic'
aics <- aics %>%
  rowid_to_column()

#AIC per model
aics %>%
  ggplot(aes(x = rowid, y = aic)) +
  geom_point() +
  ylab("AIC") + xlab("Model Index") +
  ggtitle("AIC for Simple Classification Models") +
  theme_bw()
ggsave("plots/classification/aic_summaries.png")


#top 3 model coef summaries
jpeg(file = "plots/classification/m3_coefs.jpeg")
coefplot::coefplot(M3)
dev.off()
jpeg(file = "plots/classification/m2_coefs.jpeg")
coefplot::coefplot(M2)
dev.off()
jpeg(file = "plots/classification/m9_coefs.jpeg")
coefplot::coefplot(M9)
dev.off()

#see largest coefs
sort(M3$coefficients)[-1] #regionZZ, customerOther, customerK. s04, n04, s02
sort(M2$coefficients)[-1] #xs03, xn07, xn08. xs04, xn04, xs02
sort(M9$coefficients)[-1] #k-xn04 (x2), d-xb07. d-xb08 (x2), k-xn05

summary(M3)

#save rsq, sigma on M9 and M4 for reference during Bayesian modeling
m3_rsq <- summary(M3)$aic
m2_rsq <- summary(M2)$aic
m3_sigma <- summary(M3)$sigma
m2_sigma <- summary(M2)$sigma

#save models
M3 %>% readr::write_rds('models/nbM3_class.rds')
M2 %>% readr::write_rds('models/nbM2_class.rds')

#WRITE-UP:

#Warnings:
# While training the models, two warnings flashed. The first warning, 'fitted probabilities numerically 0 or 1 occurred' is essentially saying that many of my predictions for mu were essentially 0 or 1 (2.2^e16 or something like that).
# This warning occurred for all models from M5-M9. Additionally, for M5-M6, the warning 'algorithm did not converge' occured, which indicates that there was linear separability for the model (this is not good for model performance as it
# prohibits us from making probabilistic predictions).                                            

#Best Models:
# There are actually a slew of "good models" here, though the lowest AIC models are M3, M2, and M9. Out of these, I prefer M3 the most because it has the highest AIC and no problems with linear separability of near 0/1 predictions of mu.
# The largest coefficients for M3 and M2 are actually very similar, with s04, n04, and s02 being important inputs for each of them. On the other hand, M9's most important coefficients were
# similar to its regression counterpart--interaction terms with sentiment features like Customer K-xn04 or Customer D-xb08. Thus, we can say these sentiment feature inputs seem to be fairly important!