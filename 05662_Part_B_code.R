rm(list=ls(all=TRUE)) 

#import dataset (combinedCSV3_july5)
data <- as.data.frame(combinedCSV3_july5)

library(graphics)
library(dplyr)
library(magrittr)
library(graphics)
library(stats)
library(tidyverse)
library(car)

#Creating mean sleep variable 
meansleep <- data.frame(group = names(tapply(data$sleepHours, data$participantID, mean, na.rm = TRUE)),
                        meansleep = as.numeric(tapply(data$sleepHours, data$participantID, mean, na.rm = TRUE)))

View(meansleep)
data2 <- merge(data, meansleep, by.x = "participantID", by.y = "group")

#removing repeated rows 
df <- select(data2, -sleepHours, -sleepRate, -respTime)
df <- unique(df)
View(df)

#create dummy variables for phq9Pre (depression scale)
df <- df %>%
  mutate(no_depression = ifelse(phq9Pre >= 0 & phq9Pre <= 5, 1, 0),
         mild_depression = ifelse(phq9Pre >= 6 & phq9Pre <= 9, 1, 0),
         moderate_depression = ifelse(phq9Pre >= 10 & phq9Pre <= 14, 1, 0),
         severe_depression = ifelse(phq9Pre >= 15 & phq9Pre <= 30, 1, 0))


# Create the dummy variables for stress
df <- df %>%
 mutate(low_stress = ifelse(pssPre >= 0 & pssPre <= 14, 1, 0),
         medium_stress = ifelse(pssPre >= 15 & pssPre <= 24, 1, 0),
         high_stress = ifelse(pssPre >= 25 & pssPre <= 35, 1, 0))


#descriptive stats for dummy variables

sum(df$no_depression == 1)
sum(df$mild_depression == 1)
sum(df$moderate_depression == 1)
sum(df$severe_depression == 1)

sum(df$low_stress == 1)
sum(df$medium_stress == 1)
sum(df$high_stress == 1)


#creating the model
model<- lm(cumulGPA ~ no_depression + mild_depression + moderate_depression + 
             + meansleep + low_stress + medium_stress, data = df)
summary(model)

#graphing the significant relationships 
ggplot(df, aes(x = no_depression, y = cumulGPA)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") 


#Assumption checking and outlier detection 

#check for outliers:
plot(model, which = 4)

#check linearity 
plot(model, which = 1)

#check normality
plot(model)
shapiro.test(model$residuals)

#check homogeneity 
fligner.test(model$residuals, model$fitted.values, df1 = nrow(df) - 2, df2 = nrow(df))

# homogeneity: convert the fitted.values variable to a factor variable
fitted.values.factor <- as.factor(model$fitted.values)
leveneTest(model$residuals, fitted.values.factor)


#check multicollinearity 
vif(model) 
