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


train<- read.csv("KNNimputed_train.csv")
summary(train)
head(train, 20)


###############
```{r}
eval$SEX <- factor(eval$SEX)
train$SEX <- factor(train$SEX)
eval$JOB <- factor(eval$JOB)
train$JOB <- factor(train$JOB)
eval$CAR_USE <- factor(eval$CAR_USE)
train$CAR_USE <- factor(train$CAR_USE)
eval$CAR_TYPE <- factor(eval$CAR_TYPE)
train$CAR_TYPE <- factor(train$CAR_TYPE)
eval$URBANICITY <- factor(eval$URBANICITY)
train$URBANICITY <- factor(train$URBANICITY)

eval$EDUCATION <- factor(eval$EDUCATION,
                         levels = c("<High School", "z_High School",
                                    "Bachelors", "Masters", "PhD"),
                         ordered = T)
train$EDUCATION <- factor(train$EDUCATION,
                          levels = c("<High School", "z_High School",
                                     "Bachelors", "Masters", "PhD"),
                          ordered = T)
```


#######################################################


LOGreg <- glm(TARGET_FLAG ~ ., data = train, 
              family = "binomial")
summary(LOGreg)
exp(coef(LOGreg))

names(train)

LOGreg1 <- glm(TARGET_FLAG ~ KIDSDRIV + AGE + YOJ + INCOME + CLM_FREQ + TRAVTIME + EDUCATION + SEX + CAR_USE, data = train, 
              family = "binomial")
summary(LOGreg1)
exp(coef(LOGreg1))