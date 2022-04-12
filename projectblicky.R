rm(list = ls())
dataset=read.csv('WineQT.csv')
colSums(is.na(dataset))
dataset$Id <- NULL
dataset1<-dataset
dataset2<-dataset
dataset3<-dataset
dataset4<-dataset
#############
#MLR
library(caTools)
split=sample.split(dataset$quality,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)
regressor_mlr<-lm(formula=quality~.,data = training_set)
y_pred=predict(regressor_mlr,newdata= testing_set)
new1<-data.frame(fixed.acidity=7.5,volatile.acidity=0.770,citric.acid=0.2,residual.sugar=8.1,
                 chlorides=0.098,free.sulfur.dioxide=30.0,total.sulfur.dioxide=92.0,density=0.99892,
                 pH=3.20,sulphates=0.58,alcohol=9.2)
predict(regressor_mlr,newdata = new1)
new2<-data.frame(fixed.acidity=6.8,volatile.acidity=0.570,citric.acid=0.00,residual.sugar=2.5,
                 chlorides=0.072,free.sulfur.dioxide=32.0,total.sulfur.dioxide=64.0,density=0.99491,
                 pH=3.43,sulphates=0.56,alcohol=11.2)
predict(regressor_mlr,newdata = new2)
new3<-data.frame(fixed.acidity=7.7,volatile.acidity=0.580,citric.acid=0.01,residual.sugar=1.8,
                 chlorides=0.088,free.sulfur.dioxide=12.0,total.sulfur.dioxide=18.0,density=0.99568,
                 pH=3.32,sulphates=0.56,alcohol=10.5)
predict(regressor_mlr,newdata = new3)
new4<-data.frame(fixed.acidity=7.2,volatile.acidity=0.635,citric.acid=0.07,residual.sugar=2.6,
                 chlorides=0.077,free.sulfur.dioxide=16.0,total.sulfur.dioxide=86.0,density=0.99748,
                 pH=3.51,sulphates=0.54,alcohol=9.7)
predict(regressor_mlr,newdata = new4)
new5<-data.frame(fixed.acidity=7.5,volatile.acidity=0.410,citric.acid=0.15,residual.sugar=3.7,
                 chlorides=0.104,free.sulfur.dioxide=29.0,total.sulfur.dioxide=94.0,density=0.99786,
                 pH=3.14,sulphates=0.58,alcohol=9.1)
predict(regressor_mlr,newdata = new5)
new6<-data.frame(fixed.acidity=15.9,volatile.acidity=0.360,citric.acid=0.65,residual.sugar=7.5,
                 chlorides=0.096,free.sulfur.dioxide=22.0,total.sulfur.dioxide=71.0,density=0.99760,
                 pH=2.98,sulphates=0.84,alcohol=14.9)
predict(regressor_mlr,newdata = new6)
new7<-data.frame(fixed.acidity=11.4,volatile.acidity=0.625,citric.acid=0.66,residual.sugar=6.2,
                 chlorides=0.088,free.sulfur.dioxide=6.0,total.sulfur.dioxide=24.0,density=0.99880,
                 pH=3.11,sulphates=0.99,alcohol=13.3)
predict(regressor_mlr,newdata = new7)
new8<-data.frame(fixed.acidity=8.0,volatile.acidity=0.300,citric.acid=0.63,residual.sugar=1.6,
                 chlorides=0.081,free.sulfur.dioxide=16.0,total.sulfur.dioxide=29.0,density=0.99588,
                 pH=3.30,sulphates=0.78,alcohol=10.8)
predict(regressor_mlr,newdata = new8)
new9<-data.frame(fixed.acidity=6.2,volatile.acidity=0.580,citric.acid=0.00,residual.sugar=1.6,
                 chlorides=0.065,free.sulfur.dioxide=8.0,total.sulfur.dioxide=18.0,density=0.99660,
                 pH=3.56,sulphates=0.84,alcohol=9.4)
predict(regressor_mlr,newdata = new9)
new10<-data.frame(fixed.acidity=5.6,volatile.acidity=0.605,citric.acid=0.05,residual.sugar=2.4,
                 chlorides=0.073,free.sulfur.dioxide=19.0,total.sulfur.dioxide=25.0,density=0.99258,
                 pH=3.56,sulphates=0.55,alcohol=12.9)
predict(regressor_mlr,newdata = new10)
#RMSE
library(caret)
RMSE(testing_set$quality,y_pred)
summary(regressor_mlr)
##############################
#SVR
library(e1071)
regressor_SVR=svm(formula=quality~.,data=training_set, 
                  type='eps-regression',
                  kernal='radial')
y_pred=predict(regressor_SVR,newdata = testing_set)
new1<-data.frame(fixed.acidity=7.5,volatile.acidity=0.770,citric.acid=0.2,residual.sugar=8.1,
                 chlorides=0.098,free.sulfur.dioxide=30.0,total.sulfur.dioxide=92.0,density=0.99892,
                 pH=3.20,sulphates=0.58,alcohol=9.2)
predict(regressor_SVR,newdata = new1)
new2<-data.frame(fixed.acidity=6.8,volatile.acidity=0.570,citric.acid=0.00,residual.sugar=2.5,
                 chlorides=0.072,free.sulfur.dioxide=32.0,total.sulfur.dioxide=64.0,density=0.99491,
                 pH=3.43,sulphates=0.56,alcohol=11.2)
predict(regressor_SVR,newdata = new2)
new3<-data.frame(fixed.acidity=7.7,volatile.acidity=0.580,citric.acid=0.01,residual.sugar=1.8,
                 chlorides=0.088,free.sulfur.dioxide=12.0,total.sulfur.dioxide=18.0,density=0.99568,
                 pH=3.32,sulphates=0.56,alcohol=10.5)
predict(regressor_SVR,newdata = new3)
new4<-data.frame(fixed.acidity=7.2,volatile.acidity=0.635,citric.acid=0.07,residual.sugar=2.6,
                 chlorides=0.077,free.sulfur.dioxide=16.0,total.sulfur.dioxide=86.0,density=0.99748,
                 pH=3.51,sulphates=0.54,alcohol=9.7)
predict(regressor_SVR,newdata = new4)
new5<-data.frame(fixed.acidity=7.5,volatile.acidity=0.410,citric.acid=0.15,residual.sugar=3.7,
                 chlorides=0.104,free.sulfur.dioxide=29.0,total.sulfur.dioxide=94.0,density=0.99786,
                 pH=3.14,sulphates=0.58,alcohol=9.1)
predict(regressor_SVR,newdata = new5)
new6<-data.frame(fixed.acidity=15.9,volatile.acidity=0.360,citric.acid=0.65,residual.sugar=7.5,
                 chlorides=0.096,free.sulfur.dioxide=22.0,total.sulfur.dioxide=71.0,density=0.99760,
                 pH=2.98,sulphates=0.84,alcohol=14.9)
predict(regressor_SVR,newdata = new6)
new7<-data.frame(fixed.acidity=11.4,volatile.acidity=0.625,citric.acid=0.66,residual.sugar=6.2,
                 chlorides=0.088,free.sulfur.dioxide=6.0,total.sulfur.dioxide=24.0,density=0.99880,
                 pH=3.11,sulphates=0.99,alcohol=13.3)
predict(regressor_SVR,newdata = new7)
new8<-data.frame(fixed.acidity=8.0,volatile.acidity=0.300,citric.acid=0.63,residual.sugar=1.6,
                 chlorides=0.081,free.sulfur.dioxide=16.0,total.sulfur.dioxide=29.0,density=0.99588,
                 pH=3.30,sulphates=0.78,alcohol=10.8)
predict(regressor_SVR,newdata = new8)
new9<-data.frame(fixed.acidity=6.2,volatile.acidity=0.580,citric.acid=0.00,residual.sugar=1.6,
                 chlorides=0.065,free.sulfur.dioxide=8.0,total.sulfur.dioxide=18.0,density=0.99660,
                 pH=3.56,sulphates=0.84,alcohol=9.4)
predict(regressor_SVR,newdata = new9)
new10<-data.frame(fixed.acidity=5.6,volatile.acidity=0.605,citric.acid=0.05,residual.sugar=2.4,
                  chlorides=0.073,free.sulfur.dioxide=19.0,total.sulfur.dioxide=25.0,density=0.99258,
                  pH=3.56,sulphates=0.55,alcohol=12.9)
predict(regressor_SVR,newdata = new10)

RMSE(testing_set$quality,y_pred)
R2(y_pred,testing_set$quality)
#################################################
#DT
library(rpart)
regressor_DT=rpart(formula=quality~., data=training_set)
y_pred=predict(regressor_DT, newdata = testing_set)
new1<-data.frame(fixed.acidity=7.5,volatile.acidity=0.770,citric.acid=0.2,residual.sugar=8.1,
                 chlorides=0.098,free.sulfur.dioxide=30.0,total.sulfur.dioxide=92.0,density=0.99892,
                 pH=3.20,sulphates=0.58,alcohol=9.2)
predict(regressor_DT,newdata = new1)
new2<-data.frame(fixed.acidity=6.8,volatile.acidity=0.570,citric.acid=0.00,residual.sugar=2.5,
                 chlorides=0.072,free.sulfur.dioxide=32.0,total.sulfur.dioxide=64.0,density=0.99491,
                 pH=3.43,sulphates=0.56,alcohol=11.2)
predict(regressor_DT,newdata = new2)
new3<-data.frame(fixed.acidity=7.7,volatile.acidity=0.580,citric.acid=0.01,residual.sugar=1.8,
                 chlorides=0.088,free.sulfur.dioxide=12.0,total.sulfur.dioxide=18.0,density=0.99568,
                 pH=3.32,sulphates=0.56,alcohol=10.5)
predict(regressor_DT,newdata = new3)
new4<-data.frame(fixed.acidity=7.2,volatile.acidity=0.635,citric.acid=0.07,residual.sugar=2.6,
                 chlorides=0.077,free.sulfur.dioxide=16.0,total.sulfur.dioxide=86.0,density=0.99748,
                 pH=3.51,sulphates=0.54,alcohol=9.7)
predict(regressor_DT,newdata = new4)
new5<-data.frame(fixed.acidity=7.5,volatile.acidity=0.410,citric.acid=0.15,residual.sugar=3.7,
                 chlorides=0.104,free.sulfur.dioxide=29.0,total.sulfur.dioxide=94.0,density=0.99786,
                 pH=3.14,sulphates=0.58,alcohol=9.1)
predict(regressor_DT,newdata = new5)
new6<-data.frame(fixed.acidity=15.9,volatile.acidity=0.360,citric.acid=0.65,residual.sugar=7.5,
                 chlorides=0.096,free.sulfur.dioxide=22.0,total.sulfur.dioxide=71.0,density=0.99760,
                 pH=2.98,sulphates=0.84,alcohol=14.9)
predict(regressor_DT,newdata = new6)
new7<-data.frame(fixed.acidity=11.4,volatile.acidity=0.625,citric.acid=0.66,residual.sugar=6.2,
                 chlorides=0.088,free.sulfur.dioxide=6.0,total.sulfur.dioxide=24.0,density=0.99880,
                 pH=3.11,sulphates=0.99,alcohol=13.3)
predict(regressor_DT,newdata = new7)
new8<-data.frame(fixed.acidity=8.0,volatile.acidity=0.300,citric.acid=0.63,residual.sugar=1.6,
                 chlorides=0.081,free.sulfur.dioxide=16.0,total.sulfur.dioxide=29.0,density=0.99588,
                 pH=3.30,sulphates=0.78,alcohol=10.8)
predict(regressor_DT,newdata = new8)
new9<-data.frame(fixed.acidity=6.2,volatile.acidity=0.580,citric.acid=0.00,residual.sugar=1.6,
                 chlorides=0.065,free.sulfur.dioxide=8.0,total.sulfur.dioxide=18.0,density=0.99660,
                 pH=3.56,sulphates=0.84,alcohol=9.4)
predict(regressor_DT,newdata = new9)
new10<-data.frame(fixed.acidity=5.6,volatile.acidity=0.605,citric.acid=0.05,residual.sugar=2.4,
                  chlorides=0.073,free.sulfur.dioxide=19.0,total.sulfur.dioxide=25.0,density=0.99258,
                  pH=3.56,sulphates=0.55,alcohol=12.9)
predict(regressor_DT,newdata = new10)

RMSE(testing_set$quality,y_pred)
R2(y_pred,testing_set$quality)
###########################################################################
#random forest

library(randomForest)
set.seed(1234)

regressor_RF=randomForest(x=training_set[,1:2], y=training_set$quality,ntree=19)

y_pred=predict(regressor_RF,newdata=testing_set)

new1<-data.frame(fixed.acidity=7.5,volatile.acidity=0.770,citric.acid=0.2,residual.sugar=8.1,
                 chlorides=0.098,free.sulfur.dioxide=30.0,total.sulfur.dioxide=92.0,density=0.99892,
                 pH=3.20,sulphates=0.58,alcohol=9.2)
predict(regressor_RF,newdata = new1)
new2<-data.frame(fixed.acidity=6.8,volatile.acidity=0.570,citric.acid=0.00,residual.sugar=2.5,
                 chlorides=0.072,free.sulfur.dioxide=32.0,total.sulfur.dioxide=64.0,density=0.99491,
                 pH=3.43,sulphates=0.56,alcohol=11.2)
predict(regressor_RF,newdata = new2)
new3<-data.frame(fixed.acidity=7.7,volatile.acidity=0.580,citric.acid=0.01,residual.sugar=1.8,
                 chlorides=0.088,free.sulfur.dioxide=12.0,total.sulfur.dioxide=18.0,density=0.99568,
                 pH=3.32,sulphates=0.56,alcohol=10.5)
predict(regressor_RF,newdata = new3)
new4<-data.frame(fixed.acidity=7.2,volatile.acidity=0.635,citric.acid=0.07,residual.sugar=2.6,
                 chlorides=0.077,free.sulfur.dioxide=16.0,total.sulfur.dioxide=86.0,density=0.99748,
                 pH=3.51,sulphates=0.54,alcohol=9.7)
predict(regressor_RF,newdata = new4)
new5<-data.frame(fixed.acidity=7.5,volatile.acidity=0.410,citric.acid=0.15,residual.sugar=3.7,
                 chlorides=0.104,free.sulfur.dioxide=29.0,total.sulfur.dioxide=94.0,density=0.99786,
                 pH=3.14,sulphates=0.58,alcohol=9.1)
predict(regressor_RF,newdata = new5)
new6<-data.frame(fixed.acidity=15.9,volatile.acidity=0.360,citric.acid=0.65,residual.sugar=7.5,
                 chlorides=0.096,free.sulfur.dioxide=22.0,total.sulfur.dioxide=71.0,density=0.99760,
                 pH=2.98,sulphates=0.84,alcohol=14.9)
predict(regressor_RF,newdata = new6)
new7<-data.frame(fixed.acidity=11.4,volatile.acidity=0.625,citric.acid=0.66,residual.sugar=6.2,
                 chlorides=0.088,free.sulfur.dioxide=6.0,total.sulfur.dioxide=24.0,density=0.99880,
                 pH=3.11,sulphates=0.99,alcohol=13.3)
predict(regressor_RF,newdata = new7)
new8<-data.frame(fixed.acidity=8.0,volatile.acidity=0.300,citric.acid=0.63,residual.sugar=1.6,
                 chlorides=0.081,free.sulfur.dioxide=16.0,total.sulfur.dioxide=29.0,density=0.99588,
                 pH=3.30,sulphates=0.78,alcohol=10.8)
predict(regressor_RF,newdata = new8)
new9<-data.frame(fixed.acidity=6.2,volatile.acidity=0.580,citric.acid=0.00,residual.sugar=1.6,
                 chlorides=0.065,free.sulfur.dioxide=8.0,total.sulfur.dioxide=18.0,density=0.99660,
                 pH=3.56,sulphates=0.84,alcohol=9.4)
predict(regressor_RF,newdata = new9)
new10<-data.frame(fixed.acidity=5.6,volatile.acidity=0.605,citric.acid=0.05,residual.sugar=2.4,
                  chlorides=0.073,free.sulfur.dioxide=19.0,total.sulfur.dioxide=25.0,density=0.99258,
                  pH=3.56,sulphates=0.55,alcohol=12.9)
predict(regressor_RF,newdata = new10)

RMSE(testing_set$quality,y_pred)
R2(y_pred,testing_set$quality)
########################################
#same tests, different split.

#MLR
library(caTools)
split=sample.split(dataset$quality,SplitRatio = 0.7)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)
regressor_mlr<-lm(formula=quality~.,data = training_set)
y_pred=predict(regressor_mlr,newdata= testing_set)
new1<-data.frame(fixed.acidity=7.5,volatile.acidity=0.770,citric.acid=0.2,residual.sugar=8.1,
                 chlorides=0.098,free.sulfur.dioxide=30.0,total.sulfur.dioxide=92.0,density=0.99892,
                 pH=3.20,sulphates=0.58,alcohol=9.2)
predict(regressor_mlr,newdata = new1)
new2<-data.frame(fixed.acidity=6.8,volatile.acidity=0.570,citric.acid=0.00,residual.sugar=2.5,
                 chlorides=0.072,free.sulfur.dioxide=32.0,total.sulfur.dioxide=64.0,density=0.99491,
                 pH=3.43,sulphates=0.56,alcohol=11.2)
predict(regressor_mlr,newdata = new2)
new3<-data.frame(fixed.acidity=7.7,volatile.acidity=0.580,citric.acid=0.01,residual.sugar=1.8,
                 chlorides=0.088,free.sulfur.dioxide=12.0,total.sulfur.dioxide=18.0,density=0.99568,
                 pH=3.32,sulphates=0.56,alcohol=10.5)
predict(regressor_mlr,newdata = new3)
new4<-data.frame(fixed.acidity=7.2,volatile.acidity=0.635,citric.acid=0.07,residual.sugar=2.6,
                 chlorides=0.077,free.sulfur.dioxide=16.0,total.sulfur.dioxide=86.0,density=0.99748,
                 pH=3.51,sulphates=0.54,alcohol=9.7)
predict(regressor_mlr,newdata = new4)
new5<-data.frame(fixed.acidity=7.5,volatile.acidity=0.410,citric.acid=0.15,residual.sugar=3.7,
                 chlorides=0.104,free.sulfur.dioxide=29.0,total.sulfur.dioxide=94.0,density=0.99786,
                 pH=3.14,sulphates=0.58,alcohol=9.1)
predict(regressor_mlr,newdata = new5)
new6<-data.frame(fixed.acidity=15.9,volatile.acidity=0.360,citric.acid=0.65,residual.sugar=7.5,
                 chlorides=0.096,free.sulfur.dioxide=22.0,total.sulfur.dioxide=71.0,density=0.99760,
                 pH=2.98,sulphates=0.84,alcohol=14.9)
predict(regressor_mlr,newdata = new6)
new7<-data.frame(fixed.acidity=11.4,volatile.acidity=0.625,citric.acid=0.66,residual.sugar=6.2,
                 chlorides=0.088,free.sulfur.dioxide=6.0,total.sulfur.dioxide=24.0,density=0.99880,
                 pH=3.11,sulphates=0.99,alcohol=13.3)
predict(regressor_mlr,newdata = new7)
new8<-data.frame(fixed.acidity=8.0,volatile.acidity=0.300,citric.acid=0.63,residual.sugar=1.6,
                 chlorides=0.081,free.sulfur.dioxide=16.0,total.sulfur.dioxide=29.0,density=0.99588,
                 pH=3.30,sulphates=0.78,alcohol=10.8)
predict(regressor_mlr,newdata = new8)
new9<-data.frame(fixed.acidity=6.2,volatile.acidity=0.580,citric.acid=0.00,residual.sugar=1.6,
                 chlorides=0.065,free.sulfur.dioxide=8.0,total.sulfur.dioxide=18.0,density=0.99660,
                 pH=3.56,sulphates=0.84,alcohol=9.4)
predict(regressor_mlr,newdata = new9)
new10<-data.frame(fixed.acidity=5.6,volatile.acidity=0.605,citric.acid=0.05,residual.sugar=2.4,
                  chlorides=0.073,free.sulfur.dioxide=19.0,total.sulfur.dioxide=25.0,density=0.99258,
                  pH=3.56,sulphates=0.55,alcohol=12.9)
predict(regressor_mlr,newdata = new10)
#RMSE
library(caret)
RMSE(testing_set$quality,y_pred)
R2(y_pred,testing_set$quality)
#######################################################################
#SVR
library(e1071)
regressor_SVR=svm(formula=quality~.,data=training_set, 
                  type='eps-regression',
                  kernal='radial')
y_pred=predict(regressor_SVR,newdata = testing_set)
new1<-data.frame(fixed.acidity=7.5,volatile.acidity=0.770,citric.acid=0.2,residual.sugar=8.1,
                 chlorides=0.098,free.sulfur.dioxide=30.0,total.sulfur.dioxide=92.0,density=0.99892,
                 pH=3.20,sulphates=0.58,alcohol=9.2)
predict(regressor_SVR,newdata = new1)
new2<-data.frame(fixed.acidity=6.8,volatile.acidity=0.570,citric.acid=0.00,residual.sugar=2.5,
                 chlorides=0.072,free.sulfur.dioxide=32.0,total.sulfur.dioxide=64.0,density=0.99491,
                 pH=3.43,sulphates=0.56,alcohol=11.2)
predict(regressor_SVR,newdata = new2)
new3<-data.frame(fixed.acidity=7.7,volatile.acidity=0.580,citric.acid=0.01,residual.sugar=1.8,
                 chlorides=0.088,free.sulfur.dioxide=12.0,total.sulfur.dioxide=18.0,density=0.99568,
                 pH=3.32,sulphates=0.56,alcohol=10.5)
predict(regressor_SVR,newdata = new3)
new4<-data.frame(fixed.acidity=7.2,volatile.acidity=0.635,citric.acid=0.07,residual.sugar=2.6,
                 chlorides=0.077,free.sulfur.dioxide=16.0,total.sulfur.dioxide=86.0,density=0.99748,
                 pH=3.51,sulphates=0.54,alcohol=9.7)
predict(regressor_SVR,newdata = new4)
new5<-data.frame(fixed.acidity=7.5,volatile.acidity=0.410,citric.acid=0.15,residual.sugar=3.7,
                 chlorides=0.104,free.sulfur.dioxide=29.0,total.sulfur.dioxide=94.0,density=0.99786,
                 pH=3.14,sulphates=0.58,alcohol=9.1)
predict(regressor_SVR,newdata = new5)
new6<-data.frame(fixed.acidity=15.9,volatile.acidity=0.360,citric.acid=0.65,residual.sugar=7.5,
                 chlorides=0.096,free.sulfur.dioxide=22.0,total.sulfur.dioxide=71.0,density=0.99760,
                 pH=2.98,sulphates=0.84,alcohol=14.9)
predict(regressor_SVR,newdata = new6)
new7<-data.frame(fixed.acidity=11.4,volatile.acidity=0.625,citric.acid=0.66,residual.sugar=6.2,
                 chlorides=0.088,free.sulfur.dioxide=6.0,total.sulfur.dioxide=24.0,density=0.99880,
                 pH=3.11,sulphates=0.99,alcohol=13.3)
predict(regressor_SVR,newdata = new7)
new8<-data.frame(fixed.acidity=8.0,volatile.acidity=0.300,citric.acid=0.63,residual.sugar=1.6,
                 chlorides=0.081,free.sulfur.dioxide=16.0,total.sulfur.dioxide=29.0,density=0.99588,
                 pH=3.30,sulphates=0.78,alcohol=10.8)
predict(regressor_SVR,newdata = new8)
new9<-data.frame(fixed.acidity=6.2,volatile.acidity=0.580,citric.acid=0.00,residual.sugar=1.6,
                 chlorides=0.065,free.sulfur.dioxide=8.0,total.sulfur.dioxide=18.0,density=0.99660,
                 pH=3.56,sulphates=0.84,alcohol=9.4)
predict(regressor_SVR,newdata = new9)
new10<-data.frame(fixed.acidity=5.6,volatile.acidity=0.605,citric.acid=0.05,residual.sugar=2.4,
                  chlorides=0.073,free.sulfur.dioxide=19.0,total.sulfur.dioxide=25.0,density=0.99258,
                  pH=3.56,sulphates=0.55,alcohol=12.9)
predict(regressor_SVR,newdata = new10)

RMSE(testing_set$quality,y_pred)
R2(y_pred,testing_set$quality)
######################################################################
#DT
library(rpart)
regressor_DT=rpart(formula=quality~., data=training_set)
y_pred=predict(regressor_DT, newdata = testing_set)
new1<-data.frame(fixed.acidity=7.5,volatile.acidity=0.770,citric.acid=0.2,residual.sugar=8.1,
                 chlorides=0.098,free.sulfur.dioxide=30.0,total.sulfur.dioxide=92.0,density=0.99892,
                 pH=3.20,sulphates=0.58,alcohol=9.2)
predict(regressor_DT,newdata = new1)
new2<-data.frame(fixed.acidity=6.8,volatile.acidity=0.570,citric.acid=0.00,residual.sugar=2.5,
                 chlorides=0.072,free.sulfur.dioxide=32.0,total.sulfur.dioxide=64.0,density=0.99491,
                 pH=3.43,sulphates=0.56,alcohol=11.2)
predict(regressor_DT,newdata = new2)
new3<-data.frame(fixed.acidity=7.7,volatile.acidity=0.580,citric.acid=0.01,residual.sugar=1.8,
                 chlorides=0.088,free.sulfur.dioxide=12.0,total.sulfur.dioxide=18.0,density=0.99568,
                 pH=3.32,sulphates=0.56,alcohol=10.5)
predict(regressor_DT,newdata = new3)
new4<-data.frame(fixed.acidity=7.2,volatile.acidity=0.635,citric.acid=0.07,residual.sugar=2.6,
                 chlorides=0.077,free.sulfur.dioxide=16.0,total.sulfur.dioxide=86.0,density=0.99748,
                 pH=3.51,sulphates=0.54,alcohol=9.7)
predict(regressor_DT,newdata = new4)
new5<-data.frame(fixed.acidity=7.5,volatile.acidity=0.410,citric.acid=0.15,residual.sugar=3.7,
                 chlorides=0.104,free.sulfur.dioxide=29.0,total.sulfur.dioxide=94.0,density=0.99786,
                 pH=3.14,sulphates=0.58,alcohol=9.1)
predict(regressor_DT,newdata = new5)
new6<-data.frame(fixed.acidity=15.9,volatile.acidity=0.360,citric.acid=0.65,residual.sugar=7.5,
                 chlorides=0.096,free.sulfur.dioxide=22.0,total.sulfur.dioxide=71.0,density=0.99760,
                 pH=2.98,sulphates=0.84,alcohol=14.9)
predict(regressor_DT,newdata = new6)
new7<-data.frame(fixed.acidity=11.4,volatile.acidity=0.625,citric.acid=0.66,residual.sugar=6.2,
                 chlorides=0.088,free.sulfur.dioxide=6.0,total.sulfur.dioxide=24.0,density=0.99880,
                 pH=3.11,sulphates=0.99,alcohol=13.3)
predict(regressor_DT,newdata = new7)
new8<-data.frame(fixed.acidity=8.0,volatile.acidity=0.300,citric.acid=0.63,residual.sugar=1.6,
                 chlorides=0.081,free.sulfur.dioxide=16.0,total.sulfur.dioxide=29.0,density=0.99588,
                 pH=3.30,sulphates=0.78,alcohol=10.8)
predict(regressor_DT,newdata = new8)
new9<-data.frame(fixed.acidity=6.2,volatile.acidity=0.580,citric.acid=0.00,residual.sugar=1.6,
                 chlorides=0.065,free.sulfur.dioxide=8.0,total.sulfur.dioxide=18.0,density=0.99660,
                 pH=3.56,sulphates=0.84,alcohol=9.4)
predict(regressor_DT,newdata = new9)
new10<-data.frame(fixed.acidity=5.6,volatile.acidity=0.605,citric.acid=0.05,residual.sugar=2.4,
                  chlorides=0.073,free.sulfur.dioxide=19.0,total.sulfur.dioxide=25.0,density=0.99258,
                  pH=3.56,sulphates=0.55,alcohol=12.9)
predict(regressor_DT,newdata = new10)

RMSE(testing_set$quality,y_pred)
R2(y_pred,testing_set$quality)
##################################################################################
#random forest

library(randomForest)
set.seed(1234)

regressor_RF=randomForest(x=training_set[,1:2], y=training_set$quality,ntree=29)

y_pred=predict(regressor_RF,newdata=testing_set)

new1<-data.frame(fixed.acidity=7.5,volatile.acidity=0.770,citric.acid=0.2,residual.sugar=8.1,
                 chlorides=0.098,free.sulfur.dioxide=30.0,total.sulfur.dioxide=92.0,density=0.99892,
                 pH=3.20,sulphates=0.58,alcohol=9.2)
predict(regressor_RF,newdata = new1)
new2<-data.frame(fixed.acidity=6.8,volatile.acidity=0.570,citric.acid=0.00,residual.sugar=2.5,
                 chlorides=0.072,free.sulfur.dioxide=32.0,total.sulfur.dioxide=64.0,density=0.99491,
                 pH=3.43,sulphates=0.56,alcohol=11.2)
predict(regressor_RF,newdata = new2)
new3<-data.frame(fixed.acidity=7.7,volatile.acidity=0.580,citric.acid=0.01,residual.sugar=1.8,
                 chlorides=0.088,free.sulfur.dioxide=12.0,total.sulfur.dioxide=18.0,density=0.99568,
                 pH=3.32,sulphates=0.56,alcohol=10.5)
predict(regressor_RF,newdata = new3)
new4<-data.frame(fixed.acidity=7.2,volatile.acidity=0.635,citric.acid=0.07,residual.sugar=2.6,
                 chlorides=0.077,free.sulfur.dioxide=16.0,total.sulfur.dioxide=86.0,density=0.99748,
                 pH=3.51,sulphates=0.54,alcohol=9.7)
predict(regressor_RF,newdata = new4)
new5<-data.frame(fixed.acidity=7.5,volatile.acidity=0.410,citric.acid=0.15,residual.sugar=3.7,
                 chlorides=0.104,free.sulfur.dioxide=29.0,total.sulfur.dioxide=94.0,density=0.99786,
                 pH=3.14,sulphates=0.58,alcohol=9.1)
predict(regressor_RF,newdata = new5)
new6<-data.frame(fixed.acidity=15.9,volatile.acidity=0.360,citric.acid=0.65,residual.sugar=7.5,
                 chlorides=0.096,free.sulfur.dioxide=22.0,total.sulfur.dioxide=71.0,density=0.99760,
                 pH=2.98,sulphates=0.84,alcohol=14.9)
predict(regressor_RF,newdata = new6)
new7<-data.frame(fixed.acidity=11.4,volatile.acidity=0.625,citric.acid=0.66,residual.sugar=6.2,
                 chlorides=0.088,free.sulfur.dioxide=6.0,total.sulfur.dioxide=24.0,density=0.99880,
                 pH=3.11,sulphates=0.99,alcohol=13.3)
predict(regressor_RF,newdata = new7)
new8<-data.frame(fixed.acidity=8.0,volatile.acidity=0.300,citric.acid=0.63,residual.sugar=1.6,
                 chlorides=0.081,free.sulfur.dioxide=16.0,total.sulfur.dioxide=29.0,density=0.99588,
                 pH=3.30,sulphates=0.78,alcohol=10.8)
predict(regressor_RF,newdata = new8)
new9<-data.frame(fixed.acidity=6.2,volatile.acidity=0.580,citric.acid=0.00,residual.sugar=1.6,
                 chlorides=0.065,free.sulfur.dioxide=8.0,total.sulfur.dioxide=18.0,density=0.99660,
                 pH=3.56,sulphates=0.84,alcohol=9.4)
predict(regressor_RF,newdata = new9)
new10<-data.frame(fixed.acidity=5.6,volatile.acidity=0.605,citric.acid=0.05,residual.sugar=2.4,
                  chlorides=0.073,free.sulfur.dioxide=19.0,total.sulfur.dioxide=25.0,density=0.99258,
                  pH=3.56,sulphates=0.55,alcohol=12.9)
predict(regressor_RF,newdata = new10)

RMSE(testing_set$quality,y_pred)
R2(y_pred,testing_set$quality)
