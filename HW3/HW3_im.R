if (!require("ggplot2",character.only = TRUE)) (install.packages("ggplot2",dep=TRUE))
if (!require("knitr",character.only = TRUE)) (install.packages("knitr",dep=TRUE))
if (!require("xtable",character.only = TRUE)) (install.packages("xtable",dep=TRUE))
if (!require("dplyr",character.only = TRUE)) (install.packages("dplyr",dep=TRUE))
if (!require("stringr",character.only = TRUE)) (install.packages("stringr",dep=TRUE))
library(ggplot2)
library(knitr)
library(xtable)
library(dplyr)
library(stringr)
library(tidyverse)
library(dplyr)
library(data.table)
require(data.table)
require(car)
require(corrgram)
require(ggplot2)

crime_train_df <- read.csv("https://raw.githubusercontent.com/johnm1990/DATA621/main/hw3/crime-training-data_modified.csv")
head(crime_train_df)

###ummary statistics
summary(crime_train_df)
sapply(crime_train_df, sd, na.rm=TRUE)
sapply(crime_train_df, hist, na.rm=TRUE)


##correlation matrix
install.packages("Hmisc")
library("Hmisc")
crime_train_df.rcorr = rcorr(as.matrix(crime_train_df))
crime_train_df.rcorr

install.packages("corrplot")
library(corrplot)
crime_train_df.cor = cor(crime_train_df)
corrplot(crime_train_df.cor)
correlation_matrix = crime_train_df.corr()


#correlation plot
crime_train_df.cor = cor(crime_train_df)
install.packages("corrplot")
library(corrplot)
corrplot(crime_train_df.cor)

cor_distarget <- cor.test(crime_train_df$dis, crime_train_df$target)
cor_distarget

cor_noxtarget <- cor.test(crime_train_df$nox, crime_train_df$target)
cor_noxtarget

cor_taxrad<- cor.test(crime_train_df$tax, crime_train_df$rad)
cor_taxrad

# graphs

plot(crime_train_df$tax,crime_train_df$rad)
plot(~ tax + rad, data = crime_train_df)
scatterplotMatrix(~ tax + rad, data = crime_train_df)
scatterplotMatrix(~ tax + rad, data = crime_train_df,
                  plot.points = F)

plot(crime_train_df$nox,crime_train_df$target)
plot(~ nox + target, data = crime_train_df)
scatterplotMatrix(~ nox + target, data = crime_train_df)

plot(crime_train_df$dis,crime_train_df$target)
plot(~ dis + target, data = crime_train_df)
scatterplotMatrix(~ dis + target, data = crime_train_df)


ggplot(data = crime_train_df, mapping = aes(x=crime_train_df$dis)) + 
  geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
  geom_density() +
  geom_rug() +
  labs(x='weighted mean of differences to five Boston employment centers') +
  theme_minimal()

ggplot(data = crime_train_df, mapping = aes(x=crime_train_df$lstat)) + 
  geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
  geom_density() +
  geom_rug() +
  labs(x='lower status of the population') +
  theme_minimal()

ggplot(data = crime_train_df, mapping = aes(x=crime_train_df$nox)) + 
  geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
  geom_density() +
  geom_rug() +
  labs(x='nitrogen oxides concentration') +
  theme_minimal()

ggplot(data = crime_train_df, mapping = aes(x=crime_train_df$medv)) + 
  geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
  geom_density() +
  geom_rug() +
  labs(x='medv') +
  theme_minimal()

ggplot(data = crime_train_df, mapping = aes(x=crime_train_df$indus)) + 
  geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
  geom_density() +
  geom_rug() +
  labs(x='indus') +
  theme_minimal()

ggplot(data = crime_train_df, mapping = aes(x=crime_train_df$tax)) + 
  geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
  geom_density() +
  geom_rug() +
  labs(x='tax') +
  theme_minimal()

hist(crime_train_df$zn)
hist(crime_train_df$nox)
hist(crime_train_df$rm)
hist(crime_train_df$rm)
hist(crime_train_df$lstat)
hist(crime_train_df$age)
hist(crime_train_df$rad)
hist(crime_train_df$ptratio)
hist(crime_train_df$indus)


#ttests to look at difference in means between target = 0 and target = 1, maybe to help create buckets
Ttest_targetpt<- t.test(crime_train_df$ptratio ~ crime_train_df$target)
Ttest_targetpt

Ttest_targetnox<- t.test(crime_train_df$nox ~ crime_train_df$target)
Ttest_targetnox


##transform variables


#which variables do we want to bucket?
#which do we want to turn into 0-1?
#which do we want to log transform?




crime_train_df$log_dis <- log(crime_train_df$dis)
ggplot(data = crime_train_df, mapping = aes(x=crime_train_df$log_dis)) + 
  geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
  geom_density() +
  geom_rug() +
  labs(x='logged weighted mean of differences to five Boston employment centers') +
  theme_minimal()

##this didn't really work
crime_train_df$log_tax <- log(crime_train_df$tax)
ggplot(data = crime_train_df, mapping = aes(x=crime_train_df$log_tax)) + 
  geom_histogram(aes(y=..density..),fill="bisque",color="white",alpha=0.7) + 
  geom_density() +
  geom_rug() +
  labs(x='logged tax') +
  theme_minimal()


crime_train_df$log_age <- log(crime_train_df$age)
hist(crime_train_df$log_age)

crime_train_df$log_ind <- log(crime_train_df$indus)
hist(crime_train_df$log_ind)

#bins for industry
industry <- crime_train_df[, "indus"]
head(industry)
cut(industry, 3, include.lowest=TRUE, labels=c("Low", "Med", "High"))
table(industry)


tax1 <- crime_train_df[, "tax"]
head(tax1)
tax1 <- cut(tax1, 3, include.lowest=TRUE, labels=c("Low", "Med", "High"))
table(tax1)
head(tax1)
View(tax1)

crime_train_df$tax1 <- tax1
head(crime_train_df)

table(crime_train_df$target, crime_train_df$tax1)

tax1_tar_chi <- chisq.test(crime_train_df$target, crime_train_df$tax1, correct=FALSE)
tax1_tar_chi

summary(crime_train_df)

##########################log models
logit1 <- glm(target ~ rad + nox + dis + medv + lstat, data = crime_train_df, family = "binomial")
summary(logit1)
mmps(logit1)


logit2 <- glm(target ~ zn+ indus + chas +rm + age + rad + nox + dis + medv + lstat + tax + ptratio, data = crime_train_df, family = "binomial")
summary(logit2)
mmps(logit2)

logit3 <- glm(target ~ zn+ log_ind + chas +rm + log_age + rad + nox + log_dis + medv + lstat + tax1 + ptratio, data = crime_train_df, family = "binomial")
summary(logit3)
mmps(logit3)

