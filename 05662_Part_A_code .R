rm(list=ls(all=TRUE)) 

#import data
df <- as.data.frame(MSControlPreference)

library(psych)
library(ggplot2)
library(dplyr)
library(stats)
library(car)
library(outliers)

#creating new variable 
IllnessDuration<-2023 - df$Year_Diagnosed
df$IllnessDuration <- IllnessDuration


#Defining the prototypical patient
columns <- c("Sex", "Employment", "Driving", "Marital_Status", "Highest_Degree")
modes <- list()

for (col in columns) {
  freq <- table(df[[col]])
  modes[[col]] <- names(freq)[which.max(freq)]}

colMeans(df[, c("Age", "Years_Education", "IllnessDuration")], na.rm=TRUE)


#Analysing the relationship between illness duration and employment 
# Group the data by Employment and IllnessDuration, and count the number of observations in each group
df_counts <- df %>%
  group_by(Employment, IllnessDuration) %>%
  summarize(count = n())

# Create a bar plot using ggplot2
ggplot(data=df_counts, aes(x=IllnessDuration, y=count, fill=Employment)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Number of people in each employment category by illness duration",
       x="Illness Duration", y="The number of people") + scale_y_continuous(limits=c(0, 11)) 

# Create a stacked bar chart: 
ggplot(data=df_counts, aes(x=IllnessDuration, y=count, fill=Employment)) +
  geom_bar(stat="identity") +
  labs(title="Number of people in each Employment category by Illness Duration",
       x="Illness Duration (years)", y="Number of people in each type of employment")



#Question 2: t-test
#assumption testing

#Assumption of normality 
# Subset the data by SharedDM
group1 <- subset(df, SharedDM == 0)
group2 <- subset(df, SharedDM == 1)

# Create a histogram for each group
hist(group1$NT_Memory, xlab="NT Memory Score", ylab="Frequency", main="NT Memory Scores (Group 1)")
hist(group2$NT_Memory, xlab="NT Memory Score", ylab="Frequency", main="NT Memory Scores (Group 2)")

# Create a normal probability plot for each group
qqnorm(group1$NT_Memory)
qqline(group1$NT_Memory)

qqnorm(group2$NT_Memory)
qqline(group2$NT_Memory)


# check with Shapiro Wilk test:
#Perform the Shapiro-Wilk test on group 1
shapiro.test(group1$NT_Memory)

# Perform the Shapiro-Wilk test on group 2
shapiro.test(group2$NT_Memory)

#Assumption of equal variances:
boxplot(NT_Memory ~ SharedDM, data=df) 
title(main="NT Memory Scores by group")

var.test(group1$NT_Memory, group2$NT_Memory)

# Perform the t-test
t.test(NT_Memory ~ SharedDM, data=df)


#test for outliers 
ggplot(df, aes(x = NT_Memory)) + geom_histogram(binwidth = 0.5)+ theme_classic()

ggplot(df, aes(x = "NT_Memory", y = NT_Memory)) + geom_boxplot()+ theme_classic()


lower_bound <- quantile(df$NT_Memory, 0.025)
upper_bound <- quantile(df$NT_Memory, 0.975)

outlier_ind <- which(df$NT_Memory < lower_bound | df$NT_Memory > upper_bound)
outlier_ind

#remove outliers

df_cleaned3<- df$NT_memory[-outlier_ind]
df_cleaned3 <- df[-outlier_ind, ]

#repeat t-test without outliers 
t.test(NT_Memory ~ SharedDM, data=df_cleaned3)



#assumption testing without outliers

# Subset the data by SharedDM
group1 <- subset(df_cleaned3, SharedDM == 0)
group2 <- subset(df_cleaned3, SharedDM == 1)

# Create a histogram for each group
hist(group1$NT_Memory, xlab="NT Memory Score", ylab="Frequency", main="NT Memory Scores (Group 1)")
hist(group2$NT_Memory, xlab="NT Memory Score", ylab="Frequency", main="NT Memory Scores (Group 2)")

# Create a normal probability plot for each group
qqnorm(group1$NT_Memory)
qqline(group1$NT_Memory)

qqnorm(group2$NT_Memory)
qqline(group2$NT_Memory)


# check with Shapiro Wilk test:
#Perform the Shapiro-Wilk test on group 1
shapiro.test(group1$NT_Memory)

# Perform the Shapiro-Wilk test on group 2
shapiro.test(group2$NT_Memory)

#Assumption of equal variances:
boxplot(NT_Memory ~ SharedDM, data=df_cleaned3) 
title(main="NT Memory Scores by group")

var.test(group1$NT_Memory, group2$NT_Memory)




#Repeat tests but with NT_Attention and NT_VisuoSpatial

#Attention:

hist(group1$NT_Attention, xlab="NT Attention Score", ylab="Frequency", main="NT Attention Scores (Group 1)")
hist(group2$NT_Attention, xlab="NT Attention Score", ylab="Frequency", main="NT Attention Scores (Group 2)")

# Create a normal probability plot for each group
qqnorm(group1$NT_Attention)
qqline(group1$NT_Attention)

qqnorm(group2$NT_Attention)
qqline(group2$NT_Attention)

# check with Shapiro Wilk test:
#Perform the Shapiro-Wilk test on group 1
shapiro.test(group1$NT_Attention)

# Perform the Shapiro-Wilk test on group 2
shapiro.test(group2$NT_Attention)

#Assumption of equal variances:
boxplot(NT_Attention ~ SharedDM, data=df) 
title(main="NT Attention Scores by group")

var.test(group1$NT_Attention, group2$NT_Attention)


# Perform the t-test
t.test(NT_Attention ~ SharedDM, data=df)


#checking for outliers in attention
ggplot(df, aes(x = NT_Attention)) + geom_histogram(binwidth = 0.5) + theme_classic() 

ggplot(df, aes(x = "NT_Attention", y = NT_Attention)) + geom_boxplot() + theme_classic() 

lower_bound <- quantile(df$NT_Attention, 0.025)
upper_bound <- quantile(df$NT_Attention, 0.975)

outlier_ind <- which(df$NT_Attention < lower_bound | df$NT_Attention > upper_bound)
outlier_ind

#remove outliers

df_cleaned3<- df$NT_Attention[-outlier_ind]
df_cleaned3 <- df[-outlier_ind, ]

#repeat t-test without outliers 
t.test(NT_Attention ~ SharedDM, data=df_cleaned3)




#Repeat for VisuoSpatial

hist(group1$NT_VisuoSpatial, xlab="NT VisuoSpatial Score", ylab="Frequency", main="NT VisuoSpatial Scores (Group 1)")
hist(group2$NT_Attention, xlab="NT VisuoSpatial Score", ylab="Frequency", main="NT VisuoSpatial Scores (Group 2)")

# Create a normal probability plot for each group
qqnorm(group1$NT_VisuoSpatial)
qqline(group1$NT_VisuoSpatial)

qqnorm(group2$NT_VisuoSpatial)
qqline(group2$NT_VisuoSpatial)

# check with Shapiro Wilk test:
#Perform the Shapiro-Wilk test on group 1
shapiro.test(group1$NT_VisuoSpatial)

# Perform the Shapiro-Wilk test on group 2
shapiro.test(group2$NT_VisuoSpatial)

#Assumption of equal variances:
boxplot(NT_VisuoSpatial ~ SharedDM, data=df) 
title(main="NT VisuoSpatial Scores by group")

var.test(group1$NT_VisuoSpatial, group2$NT_VisuoSpatial)


# Perform the t-test
t.test(NT_VisuoSpatial ~ SharedDM, data=df)



#checking for outliers in VisuoSpatial
ggplot(df, aes(x = NT_VisuoSpatial)) + geom_histogram(binwidth = 0.5)+ theme_classic()

ggplot(df, aes(x = "NT_VisuoSpatial", y = NT_VisuoSpatial)) + geom_boxplot()+ theme_classic()

lower_bound <- quantile(df$NT_VisuoSpatial, 0.025)
upper_bound <- quantile(df$NT_VisuoSpatial, 0.975)

outlier_ind <- which(df$NT_VisuoSpatial < lower_bound | df$NT_VisuoSpatial > upper_bound)
outlier_ind

#remove outliers

df_cleaned3<- df$NT_VisuoSpatial[-outlier_ind]
df_cleaned3 <- df[-outlier_ind, ]

#repeat t-test without outliers 
t.test(NT_VisuoSpatial ~ SharedDM, data=df_cleaned3)



#Question 3

#key: 
#model = single linear regression with outliers 
#model_clean = single linear regression without outliers 
#model2 = multiple linear regression with outliers 
#model2.clean = multiple linear regression without outliers 

# Load required packages
library(ggplot2)

# Fit the linear model
model <- lm(Brief_Illness ~ Managing_Meds, data = df)

# Extract the model predictions and the original data
predictions <- data.frame(Managing_Meds = df$Managing_Meds, 
                          model = predict(model))


summary(model)

# Create a scatterplot of the original data
plot <- ggplot(data = df, aes(x = Managing_Meds, y = Brief_Illness)) +
  geom_point()

# Alter the plot
plot <- plot + geom_line(data = predictions, aes(x = Managing_Meds, y = model), 
                         color = "red")

plot <- plot + labs(title = "Illness Perception vs. Disease Management",
                    x = "Disease Management (Confidence in managing medication)", y = "Illness Perception of severity")

#view
plot




#checking and removing outliers 
residuals <- model$residuals

boxplot(residuals)
outlier_ind <- which(df$Brief_Illness > boxplot.stats(df$Brief_Illness)$stats[5] | df$Brief_Illness < boxplot.stats(df$Brief_Illness)$stats[1])
outlier_ind

# remove the outliers from the data
df_cleaned <- df[-outlier_ind, ]

# create a multiple linear regression model using the modified data
model_clean <- lm(Brief_Illness ~ Managing_Meds, data = df_cleaned)


#model_clean = single linear regression without outliers 




#question 3 part 2

model2 <- lm(Brief_Illness ~ Managing_Meds + SharedDM, data = df)
summary(model2)

# Extract the model predictions and the original data
predictions <- data.frame(Managing_Meds = df$Managing_Meds, SharedDM = df$SharedDM,
                          model2 = predict(model2))

# Create a faceted plot with separate panels for each level of SharedDM
plot <- ggplot(data = df, aes(x = Managing_Meds, y = Brief_Illness)) +
  geom_point() +
  geom_line(data = predictions, aes(x = Managing_Meds, y = model2), color = "red") +
  facet_wrap(~SharedDM)

# Add a title and labels to the plot
plot <- plot + labs(title = "Illness Perception vs. Disease Management",
                    x = "Disease Management", y = "Illness Perception")

# Display the plot
plot




#remove outliers and repeat the multiple linear model analysis

# fit a multiple linear regression model
model2 <- lm(Brief_Illness ~ Managing_Meds + SharedDM, data = df)

# compute Cook's distances for the model
cooks.distance(model2)

# identify the indices of the outliers in the data
outlier.indices <- which(cooks.distance(model2) > 4 / nrow(df))

# remove the outliers from the data
df.clean <- df[-outlier.indices, ]

# refit the multiple linear regression model without the outliers
model2.clean <- lm(Brief_Illness ~ Managing_Meds + SharedDM, data = df.clean)





#checking assumptions of linear regression 
plot(model)
plot(model_clean)
plot(model2)
plot(model2.clean)

#checking linearity 
plot(model, 1)
plot(model_clean, 1)
plot(model2, 1)
plot(model2.clean, 1)

#checking multicollinearity (only for multiple linear regression)
vif(model2) 
vif(model2.clean) 

#normality
shapiro.test(model$residuals)
shapiro.test(model_clean$residuals)
shapiro.test(model2$residuals)
shapiro.test(model2.clean$residuals)

#Assumption of homogeneity 
fligner.test(model$residuals, model$fitted.values, df1 = nrow(df) - 2, df2 = nrow(df))
fligner.test(model_clean$residuals, model_clean$fitted.values, df1 = nrow(df) - 2, df2 = nrow(df))

fligner.test(model2$residuals, model2$fitted.values, df1 = nrow(df) - 2, df2 = nrow(df))
fligner.test(model2.clean$residuals, model2.clean$fitted.values, df1 = nrow(df.clean) - 2, df2 = nrow(df.clean))


#Checking for outliers 
boxplot(resid(model))
boxplot(resid(model_clean))
boxplot(resid(model2))
boxplot(resid(model2.clean))

#checking for other influential points 
plot(model, 5)
plot(model_clean, 5)
plot(model2, 5)
plot(model2.clean, 5)




#Question 4 

#rescaling illness_perception 
df$PDDSZScore<-rescale(df$PDDS, mean = 0, sd = 1,df=FALSE)
df$MSIS_GlobalZScore<-rescale(df$MSIS_Global, mean = 0, sd = 1,df=FALSE)
df$Brief_IllnessZScore<-rescale(df$Brief_Illness, mean = 0, sd = 1,df=FALSE)

df$AggregateIllnessPerception<-rowMeans(cbind(df$PDDSZScore,df$MSIS_GlobalZScore,
                                              df$Brief_IllnessZScore))


#rescaling cognitive ability
df$NT_MemoryZScore<-rescale(df$NT_Memory, mean = 0, sd = 1,df=FALSE)
df$NT_AttentionZScore<-rescale(df$NT_Attention, mean = 0, sd = 1,df=FALSE)
df$NT_VisuoSpatialZScore<-rescale(df$NT_VisuoSpatial, mean = 0, sd = 1,df=FALSE)


df$AggregateCognitiveAbility<-rowMeans(cbind(df$NT_MemoryZScore,df$NT_AttentionZScore,
                                             df$NT_VisuoSpatialZScore))

#rescaling mental health 
df$PHQ.2ZScore<-rescale(df$PHQ.2, mean = 0, sd = 1,df=FALSE)
df$StigmaZScore<-rescale(df$Stigma, mean = 0, sd = 1,df=FALSE)
df$UCLA.3ZScore<-rescale(df$UCLA.3, mean = 0, sd = 1,df=FALSE)


df$AggregateMentalHealth<-rowMeans(cbind(df$PHQ.2ZScore,df$StigmaZScore,
                                         df$UCLA.3ZScore))

#correlation matrix

correlation_matrix <- cor(df[,c("AggregateMentalHealth", "AggregateCognitiveAbility", "AggregateIllnessPerception")])

pairs(df[,c("AggregateMentalHealth", "AggregateCognitiveAbility", "AggregateIllnessPerception")])

#check significance:

cor.test(df$AggregateMentalHealth, df$AggregateCognitiveAbility)
cor.test(df$AggregateMentalHealth, df$AggregateIllnessPerception)
cor.test(df$AggregateIllnessPerception, df$AggregateCognitiveAbility)
