#Sean Steinle
#CS1675 Final Project
#Exploratory Data Analysis

#First, let's review our task in this notebooks per the guidelines document:

# Part i: Exploration
# •
# Visualize the distributions of variables in the data set. x
# •
# Counts for categorical variables. X
# •
# Distributions for continuous variables. Are the distributions Gaussian like? (For X too?)
#   •
# Consider conditioning (grouping or “breaking up”) the continuous variables based (For X too?)
# on the categorical variables.
# •
# Are there differences in continuous variable distributions and continuous variable summary 
# statistics based on region or customer?
#   •
# Are there differences in continuous variable distributions and continuous variable summary
# statistics based on the binary outcome?
#   •
# Visualize the relationships between the continuous inputs, are they correlated?
#   •
# Visualize the relationships between the continuous outputs ( response and the
#                                                              log transformed response ) with respect to the continuous inputs.
# •
# Can you identify any clear trends? Do the trends depend on the categorical inputs?
#   •
# How can you visualize the behavior of the binary outcome with respect to the
# continuous inputs?


#FACTORS AND BOTH RESPONSES:
#DONE:
#BASIC SUMMARIES OF FACTORS.
#SUMMARIZED RESPONSE1 WITH RUN CHARTS AND HISTOGRAMS. IT IS GAUSSIAN.
#SUMMARIZED RESPONSE2 WITH RUN CHARTS AND HISTOGRAMS. REGION SEEMS TO BE PLAYING A ROLE.
#NEED TO DO:
#SUMMARIZE NUMERICALLY AS WELL

#PREDICTORS:
#DONE:
#SUMMARIZED EACH GROUP VS. R1 ALONE, BY REGION, BY CUSTOMER
#NEED TO DO:
#SUMMARIZE DISTRIBUTIONS INDEPENDENTLY
#SUMMARIZE WITH RESPECT TO R2 ALONE, BY REGION, BY CUSTOMER

#OVERALL:
#NEED TO DO:
#CHECK FILE OUTPUT NAMES


#MORE HISTOGRAMS (FOR CONTINOUS INPUTS!) are they gaussian?
  #HISTS BY CUSTOMER/REGION
#CORRELATION

#LOADING DATA
df <- readr::read_csv("data/final_project_train.csv", col_names = TRUE)
colnames(df)

# ABOUT THE DATA
# ROWS: Each row in our data is an attempted sale (project) for PPG. Each sale has a unique rowid number associated with it.
#
# FACTORS: For every sale, we have data on the region that we're selling to and the customer that we're selling to 
# (lots of repeat customers). These are our categorical variables.

#HISTOGRAMS (FACTORS)
df %>% #REGION
  ggplot()  +
  geom_histogram(aes(x = region, y = stat(count/sum(count))), stat = 'count') +
  ggtitle("Distribution of Region in Sales Projects") +
  xlab("Region") + ylab("Proportion of Total Sales Projects") +
  theme_minimal()
ggsave("plots/EDA/region_dist.png")
  

#There are three regions, which have somewhat of an equal balance between them.

df %>% #CUSTOMER
  ggplot()  +
  geom_histogram(aes(x = customer, y = stat(count/sum(count))), stat = 'count') +
  ggtitle("Distribution of Customer in Sales Projects") +
  xlab("Customer") + ylab("Proportion of Total Projects") +
  theme_minimal()
ggsave("plots/EDA/customers_dist.png")

#We have eight main customers plus an "other" category. The main customers make up over half of the data, with our main customers being targets for 40-100 projects each.

# RESPONSES: For every sale, we also have data on how many hours per week our sales rep invested in selling the product and whether or not the product hit its sales goal. These
# are the response variables (for regression and classification respectively).

#RESPONSE 1 RUN CHARTS:
df %>% #RESPONSE (CONTINUOUS, TASK 1) #Woah, look at that outlier!
  ggplot(aes(x = rowid, y = response)) + 
  geom_point() +
  ggtitle("Average Hours Worked Per Week By Project Index") +
  xlab("Project Index") + ylab("Average Hours Worked Per Week") +
  theme_minimal()
ggsave("plots/EDA/response_run_chart.png")

df %>% #RESPONSE BY REGION
  ggplot(aes(x = rowid, y = response, col = region)) + 
  geom_point() +
  ggtitle("Average Hours Worked Per Week By Project Index") +
  xlab("Project Index") + ylab("Average Hours Worked Per Week") +
  theme_minimal()
ggsave("plots/EDA/response_run_chart_byregion.png")

df %>% #RESPONSE BY CUSTOMER
  ggplot(aes(x = rowid, y = response, col = customer)) + 
  geom_point() +
  ggtitle("Average Hours Worked Per Week By Project Index") +
  xlab("Project Index") + ylab("Average Hours Worked Per Week") +
  theme_minimal()
ggsave("plots/EDA/response_run_chart_bycustomer.png")

df %>% #RESPONSE IN LOG SPACE
  ggplot(aes(x = rowid, y = log(response))) + 
  geom_point() +
  ggtitle("Log Average Hours Worked Per Week By Project Index") +
  xlab("Project Index") + ylab("Log Average Hours Worked Per Week") +
  theme_minimal()
ggsave("plots/EDA/log_response_run_chart.png")

#Most values stay positive in the log space, but still need to respect bounds! We'll consider distributions in log-space because we want to observe normality in our actual
#output space!

#RESPONSE 1 HISTOGRAMS
df %>% #RESPONSE
  ggplot() + 
  geom_histogram(aes(x = response, y = stat(count)), stat = 'bin', bins = 30) +
  ggtitle("Average Hours Worked Per Week Distribution") +
  xlab("Average Hours Worked Per Week") + ylab("Count") +
  theme_minimal()
ggsave("plots/EDA/response_dist.png")

df %>% #RESPONSE IN LOG SPACE
  ggplot() + 
  geom_histogram(aes(x = log(response), y = stat(count)), stat = 'bin', bins = 30) +
  ggtitle("Log Average Hours Worked Per Week Distribution") +
  xlab("Log Average Hours Worked Per Week") + ylab("Count") +
  theme_minimal()
ggsave("plots/EDA/log_response_dist.png")

df %>% #RESPONSE IN LOG SPACE BY REGION
  ggplot() + 
  geom_histogram(aes(x = log(response), y = stat(count)), stat = 'bin', bins = 20) +
  ggtitle("Log Average Hours Worked Per Week Distributions By Region") +
  xlab("Log Average Hours Worked Per Week") + ylab("Count") +
  facet_wrap(~region) +
  theme_minimal()
ggsave("plots/EDA/log_response_byregion_dist.png")

df %>% #RESPONSE IN LOG SPACE BY CUSTOMER
  ggplot() + 
  geom_histogram(aes(x = log(response), y = stat(count)), stat = 'bin', bins = 9) +
  ggtitle("Log Average Hours Worked Per Week Distributions By Customer") +
  xlab("Log Average Hours Worked Per Week") + ylab("Count") +
  facet_wrap(~customer) +
  theme_minimal()
ggsave("plots/EDA/log_response_bycustomer_dist.png")

#Looks like our response is very normal! Tricky to tell for customers because less data, but looks largely normal. Nothing concerning.

#RESPONSE 2 RUN CHART
df %>% #OUTCOME 
  ggplot() + 
  geom_point(aes(x = rowid, y = outcome)) +
  ggtitle("Run Chart of Sales Targets Hit or Missed") +
  xlab("Project Index") + ylab("Project Outcome") +
  theme_minimal()
ggsave("plots/EDA/outcome_run_chart.png")

#RESPONSE 2 HISTOGRAMS
df %>% #OUTCOME (BINARY, TASK 2)
  ggplot() + 
  geom_histogram(aes(x = outcome, y = stat(count/sum(count))), stat = "count") +
  ggtitle("Distribution of Sales Targets Hit vs. Missed") +
  xlab("Group") + ylab("Proportion of Total Sales Projects") +
  theme_minimal()
ggsave("plots/EDA/outcome_dist.png")

df %>% #OUTCOME BY REGION
  ggplot() + 
  geom_histogram(aes(x = outcome, y = stat(count/sum(count))), stat = "count") +
  ggtitle("Distribution of Sales Targets Hit vs. Missed By Region") +
  xlab("Group") + ylab("Proportion of Total Sales Projects") +
  facet_wrap(~region) +
  theme_minimal()
ggsave("plots/EDA/outcome_dist_byregion.png")

df %>% #OUTCOME BY REGION
  ggplot() + 
  geom_histogram(aes(x = outcome, y = stat(count/sum(count))), stat = "count") +
  ggtitle("Distribution of Sales Targets Hit vs. Missed By Customer") +
  xlab("Group") + ylab("Proportion of Total Sales Projects") +
  facet_wrap(~customer) +
  theme_minimal()
ggsave("plots/EDA/outcome_dist_bycustomer.png")

#Interestingly, it looks like region may be a useful feature for classification. ZZ has a much lower success rate than XX.

# PREDICTORS: Finally, the majority of our columns are predictors. For each sale, we have five sets of lexically derived features 
# (AFINN: xa, Bing: xb, NRC: xn, sentimentr: xs, count-based: xw). xa, xb, and xn have 8 features each, xs has 6 features, and xw has 3 features.

#Let's see how many predictors we have in each set, then we can make separate DFs each for graphical purposes.
cols <- names(df)
length(cols[substr(cols, 1, 2) == "xa"])
length(cols[substr(cols, 1, 2) == "xb"])
length(cols[substr(cols, 1, 2) == "xn"])
length(cols[substr(cols, 1, 2) == "xs"])
length(cols[substr(cols, 1, 2) == "xw"])

df_xa <- data.frame(y = df$response, #this extends our DF longer by reducing many attributes to 1 identifying attribute for each row
                    x = c(df$xa_01, df$xa_02, df$xa_03, df$xa_04, df$xa_05, df$xa_06, df$xa_07, df$xa_08),
                    group = c(rep("a1", nrow(df)),
                              rep("a2", nrow(df)),
                              rep("a3", nrow(df)),
                              rep("a4", nrow(df)),
                              rep("a5", nrow(df)),
                              rep("a6", nrow(df)),
                              rep("a7", nrow(df)),
                              rep("a8", nrow(df))))

df_xb <- data.frame(y = df$response, #this extends our DF longer by reducing many attributes to 1 identifying attribute for each row
                    x = c(df$xb_01, df$xb_02, df$xb_03, df$xb_04, df$xb_05, df$xb_06, df$xb_07, df$xb_08),
                    group = c(rep("b1", nrow(df)),
                              rep("b2", nrow(df)),
                              rep("b3", nrow(df)),
                              rep("b4", nrow(df)),
                              rep("b5", nrow(df)),
                              rep("b6", nrow(df)),
                              rep("b7", nrow(df)),
                              rep("b8", nrow(df))))

df_xn <- data.frame(y = df$response, #this extends our DF longer by reducing many attributes to 1 identifying attribute for each row
                    x = c(df$xn_01, df$xn_02, df$xn_03, df$xn_04, df$xn_05, df$xn_06, df$xn_07, df$xn_08),
                    group = c(rep("n1", nrow(df)),
                              rep("n2", nrow(df)),
                              rep("n3", nrow(df)),
                              rep("n4", nrow(df)),
                              rep("n5", nrow(df)),
                              rep("n6", nrow(df)),
                              rep("n7", nrow(df)),
                              rep("n8", nrow(df))))

df_xs <- data.frame(y = df$response, #this extends our DF longer by reducing many attributes to 1 identifying attribute for each row
                    x = c(df$xs_01, df$xs_02, df$xs_03, df$xs_04, df$xs_05, df$xs_06),
                    group = c(rep("s1", nrow(df)),
                              rep("s2", nrow(df)),
                              rep("s3", nrow(df)),
                              rep("s4", nrow(df)),
                              rep("s5", nrow(df)),
                              rep("s6", nrow(df))))

df_xw <- data.frame(y = df$response, #this extends our DF longer by reducing many attributes to 1 identifying attribute for each row
                    x = c(df$xw_01, df$xw_02, df$xw_03),
                    group = c(rep("w1", nrow(df)),
                              rep("w2", nrow(df)),
                              rep("w3", nrow(df))))

#AFINN
df_xa %>%
  ggplot(aes(x, y, col = group)) +
  geom_point(alpha = 0.5) +
  ggtitle("AFINN Sentiment Features vs. Hours Worked") +
  xlab("X") + ylab("Y") + 
  theme_minimal()
ggsave("plots/EDA/afeats_response.png")

#LOT of overlap here. Still can tell general trends of a8 tends towards 0, a2 tends towards large values.

#Bing Features
df_xb %>%
  ggplot(aes(x, y, col = group)) +
  geom_point(alpha = 0.5) +
  ggtitle("Bing Sentiment Features vs. Hours Worked") +
  xlab("X") + ylab("Y") +
  theme_minimal()
ggsave("plots/EDA/bfeats_response.png")

#Looks like these are somewhat discretized?

#NRC Features
df_xn %>%
  ggplot(aes(x, y, col = group)) +
  geom_point(alpha = 0.5) +
  ggtitle("NRC Sentiment Features vs. Hours Worked") +
  xlab("X") + ylab("Y") +
  theme_minimal()
ggsave("plots/EDA/nfeats_response.png")

#Also somewhat discretized, very active inside x = (-1,3)

#sentimentr Features

df_xs %>%
  ggplot(aes(x, y, col = group)) +
  geom_point(alpha = 0.5) +
  ggtitle("sentimentr Sentiment Features vs. Hours Worked") +
  xlab("X") + ylab("Y") +
  theme_minimal()
ggsave("plots/EDA/sfeats_response.png")

#Not discrete at all, though all features except s2/s3 non-negative!

#Count-Based Features
df_xw %>%
  ggplot(aes(x, y, col = group)) +
  geom_point(alpha = 0.5) +
  ggtitle("Word Count-Based Sentiment Features vs. Hours Worked") +
  xlab("X") + ylab("Y") +
  theme_minimal()
ggsave("plots/EDA/wfeats_response.png")

#Wow, very different from our other X sets. w2 small, w1 middle, w3 big. Larger scale than our other 4 sets.