## loading libraries
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(plotly)
library(caret) 
library(AppliedPredictiveModeling)
##library(caret)
## set working space 
setwd("D:/Data analysis/code/R/R introduction/Wine")
## loading data 
data <- read.csv("winequality-red.csv", header = TRUE,sep = ";")
##summary of data
head(data)

str(data)
summary(data)
 
## intializing a theme for ggplot
my_theme <- theme_bw() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))


my_theme_dark <- theme_dark() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

## display the quality histogram
ggplot(data, aes(x = quality)) +
         geom_histogram()+
  my_theme

## display various histograms of all the predictors:
## chloride
ggplot(data, aes(x = chlorides)) +
  geom_histogram()+
  my_theme
##alchohol
ggplot(data, aes(x = alcohol)) +
  geom_histogram()+
  my_theme
## volatile.acidity
ggplot(data, aes(x = volatile.acidity)) +
  geom_histogram()+
  my_theme
##fixed.acidity
ggplot(data, aes(x = fixed.acidity)) +
  geom_histogram()+
  my_theme
##citric.acid
ggplot(data, aes(x = citric.acid)) +
  geom_histogram()+
  my_theme
##residual.sugar
ggplot(data, aes(x = residual.sugar )) +
  geom_histogram()+
  my_theme
##free.sulfur.dioxide
ggplot(data, aes(x = free.sulfur.dioxide)) +
  geom_histogram()+
  my_theme
##total.sulfur.dioxide
ggplot(data, aes(x = total.sulfur.dioxide)) +
  geom_histogram()+
  my_theme
##density
ggplot(data, aes(x = density)) +
  geom_histogram()+
  my_theme
##sulphates
ggplot(data, aes(x = sulphates)) +
  geom_histogram()+
  my_theme
##pH
ggplot(data, aes(x = pH)) +
  geom_histogram()+
  my_theme

## facets of histogram of all variables
td = gather(data,-quality, key = "key", value = "value")
ggplot(td, aes(x = value)) +
 geom_histogram(fill = "grey20", binwidth = .01) +
 
  facet_wrap(~key , scales = "free")
td2 = gather(data,-quality,-pH, key = "key", value = "value")
ggplot(td, aes(x = quality,y = value)) +
  geom_point(alpha =0.4) +
  geom_smooth(method = "lm")+
  facet_wrap(~key , scales = "free")
data$alcohol_volatile.acidity = data$alcohol*data$volatile.acidity
cor(log(data$alcohol^100/data$residual.sugar),data$quality)
M <- abs(cor(data))
scatter(M)
ld <- gather(data, numerical, value, exmtcars %>%
               gather(-mpg, -hp, -cyl, key = "var", value = "value") %>% traversion:conscientiousness)
ld

d = mtcars
   gather(data=d,-mpg, -hp, -cyl, key = "var", value = "value") 
  ggplot(aes(x = value, y = mpg, color = hp, shape = factor(cyl))) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()
  train <- data[1:1000,]
  test <- data[1001:1599,]

  
  #####################################
  summary(data)
  data2 = data;
  data2$quality[data$quality>=5] = 1
  data2$quality[data$quality<5] = 0
  summary(data2)
  train2 = caret::createDataPartition(y=data2$quality,list = F,times=1,p=0.75)
    
  trainset2 = data2[train2,]
  testset2 = data2[-train2,]
  
   mode<- train(quality~.,data= trainset2, method = "gbm", trControl = trainControl(method = "repeatedcv",number = 10, repeats = 5))
   p =predict(mode, testset2)  
  
     sum(abs(round(p)-testset2$quality))/length(p)
      data3 = data
      data3$quality[data$quality>=5] = NA
     data2$quality[data$quality<5] = 0
    data3 <- na.omit(data3)      
    data3$quality[data3$quality==4] = 1
    data3$quality[data3$quality==3] = 0
    summary(data3)
    train3 = caret::createDataPartition(y=data3$quality,list = F,times=1,p=0.75)
    
    trainset3 = data3[train3,]
    testset3 = data3[-train3,]
    
    mode2<- train(quality~.,data= trainset3, method = "gbm", trControl = trainControl(method = "repeatedcv",number = 10, repeats = 5))
    p2 =predict(mode2, testset3)  
    
    sum(abs(round(p2)-testset3$quality))/length(p2)
    data4=data
    data4$quality[data$quality>=5] = 0
    data4$quality[data$quality<5] = NA
    data4 <- na.omit(data4)      
    data4$quality[data4$quality==4] = 1
    data4$quality[data4$quality==3] = 0
    summary(data4)
    train4 = caret::createDataPartition(y=data4$quality,list = F,times=1,p=0.75)
    
    trainset4 = data4[train3,]
    testset4 = data4[-train3,]
    
    mode3<- train(quality~.,data= trainset4, method = "gbm", trControl = trainControl(method = "repeatedcv",number = 10, repeats = 5))
    p3 =predict(mode3, testset4)  
    
    sum(abs(round(p3)-testset3$quality))/length(p2)
    
    
    
    
    
      
args(trainControl)
names(getModelInfo())
pairs.panels()
library(psych)
pairs.panels(data)
 args(lm)
lm1 <- lm(quality~., data= data)
summary(lm1)
pairs(quality ~ log(volatile.acidity), data= data)
pairs(quality ~ log(chlorides), data= data)
pairs(quality ~ log(sulphates), data= data)

data2$volatile.aciditity <- log(data$volatile.acidity)
data2$alcohol <- log(data$alcohol)
data2$sulphates <- log(data$sulphates)
data2$alcohol <- log(data$alcohol)
library(rpart)
attach(data)
train = caret::createDataPartition(y=data$quality,list = F,times=1,p=0.75)
train
trainset = data[train,]
testset = data[-train,]
str(trainset)
str(testset)
model <- rpart(quality~.,data = trainset)
summary(model)
library(rpart.plot)
r.part = predict(model, testset)
r.part =round(r.part)+0.5
r.part
sum(abs(testset$quality-r.part))/363
cor(r.part,testset$quality)
