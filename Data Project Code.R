library(tree)
library(ISLR)

library(MASS)
library(randomForest)
library(MASS)

library(gbm)

data = read.csv("Bike_Rental_Data.csv", header=T, sep=",")

#Decision Tree and Random Forest with bagging

data_subset <- data[,c("holiday", "atemp", "humidity", "windspeed", "hour", "dayofweek", "timediff", "season", "weather")]

count1 = data[,c("count")]

#Decision Tree

tree1 = tree(count1~. ,data_subset)

cv1 = cv.tree(tree1)
plot(cv1$size, cv1$dev, type='b')

tree2 = prune.tree(tree1, best = 10)
plot(tree2)
text(tree2)

pred2 = predict(tree2, newdata = data_subset)

#MSE
mean((pred2-count1)^2)

#R^2

r2_decision_tree <- 1-sum((pred2-count1)^2)/sum((count1-mean(count1))^2)
r2_decision_tree



#Bagging

train=sample(1:nrow(data_subset), nrow(data_subset)*0.8)

Bikes.test=data_subset[-train,]
count1.test=count1[-train]



#Bagging number of trees

MSE_bag <- array(0,dim=c(15))
r2_bag <- array(0,dim=c(15))
Number_Of_Trees <- array(0,dim=c(15))

for (i in 1:15){
  Number_Of_Trees [i] <- 10*i
}

for (i in 1:15){
  rf.bike4=randomForest(count1~.,data=data_subset,subset=train, mtry=9, ntree=10*i, importance=TRUE)
  pred_bag4=predict(rf.bike4,Bikes.test)
  MSE_bag[i] = mean((pred_bag4-count1.test)^2)
  r2_bag[i] <- 1-sum((pred_bag4-count1.test)^2)/sum((count1.test-mean(count1))^2)
}

plot(Number_Of_Trees , MSE_bag, type='b')
r2_bag
MSE_bag

#10 Trees

rf.bike1=randomForest(count1~.,data=data_subset,subset=train, mtry=9, ntree=10, importance=TRUE)
pred_bag1=predict(rf.bike1,Bikes.test)

#MSE
mean((pred_bag1-count1.test)^2)

#R^2

r2_bag1 <- 1-sum((pred_bag1-count1.test)^2)/sum((count1.test-mean(count1))^2)
r2_bag1

#50 Trees
rf.bike2=randomForest(count1~.,data=data_subset,subset=train, mtry=9, ntree=50, importance=TRUE)
pred_bag2=predict(rf.bike2,Bikes.test)

#MSE
mean((pred_bag2-count1.test)^2)

#R^2

r2_bag2 <- 1-sum((pred_bag2-count1.test)^2)/sum((count1.test-mean(count1))^2)
r2_bag2


#100 Trees
rf.bike3=randomForest(count1~.,data=data_subset,subset=train, mtry=9, ntree=100, importance=TRUE)
pred_bag3=predict(rf.bike3,Bikes.test)

#MSE
mean((pred_bag3-count1.test)^2)

#R^2

r2_bag3 <- 1-sum((pred_bag3-count1.test)^2)/sum((count1.test-mean(count1))^2)
r2_bag3


#Random Forest

#100 Trees


MSE_100 <- array(0,dim=c(9))
r2_100 <- array(0,dim=c(9))

for (i in 1:9){
  rf.bike4=randomForest(count1~.,data=data_subset,subset=train, mtry=i, ntree=100, importance=TRUE)
  pred_bag4=predict(rf.bike4,Bikes.test)
  MSE_100[i] = mean((pred_bag4-count1.test)^2)
  r2_100[i] <- 1-sum((pred_bag4-count1.test)^2)/sum((count1.test-mean(count1))^2)
}

MSE_100
r2_100

Number_Of_Variables <- array(0,dim=c(9))

for (i in 1:9){
  Number_Of_Variables [i] <- i
}

plot(Number_Of_Variables , MSE_100, type='b')

rf.bike5=randomForest(count1~.,data=data_subset,subset=train, mtry=7, ntree=100, importance=TRUE)

#Variable Importance
importance(rf.bike5)
varImpPlot(rf.bike5)
