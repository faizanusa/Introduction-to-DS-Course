data(bodyfat)
head(bodyfat)
df<-subset(bodyfat, select=c(age, DEXfat, waistcirc, hipcirc, elbowbreadth, kneebreadth))
head(df)
summary(df)
?bodyfat
dim(df)
#train test split
index<-sample(1:nrow(df), round(0.75*nrow(df)))
head(index)
train<-df[index,]
test<-df[-index, ]
#multiple regression
MR<-lm(DEXfat~., data=train)
summary(MR)
#MSE
pred<-predict(MR, newdata=test)
head(pred)
MSEmr<-sum((pred - test$DEXfat)^2)/nrow(test)
MSEmr
#NEuralNet 3 nodes in hidden layer
#step1:max and min column wise
Maxs<-apply(df, 2, max)
Maxs
Mins<-apply(df, 2, min)
Mins
Scaleddf<-scale(df, center=Mins, scale=Maxs-Mins)
class(Scaleddf)
head(Scaleddf)
#step2, select train set and test set
head(index)
trainset<-Scaleddf[index, ]
testset<-Scaleddf[-index, ]
dim(trainset)
dim(testset)
#neural network with three hidden nodes
Neural1<-neuralnet(DEXfat~., data=trainset, hidden=3, linear.output=TRUE)
Neural1
names(Neural1)
print(Neural1)
plot(Neural1)
Pred1<-predict(Neural1, newdata=testset)
Pred1
Pred1Orig<-(Pred1)*(max(df$DEXfat)-min(df$DEXfat))+min(df$DEXfat)
(Pred1Orig)
testDEXfat1<-(testset[, 2])*(max(df$DEXfat)-min(df$DEXfat))+min(df$DEXfat)
(testDEXfat1)
MSENEural1<-sum((Pred1Orig-testDEXfat1)^2)/18
MSENEural1
#neural network with four hidden nodes
Neural2<-neuralnet(DEXfat~., data=trainset, hidden=4, linear.output=TRUE)
Neural2
plot(Neural2)
Pred2<-predict(Neural2, newdata=testset)
Pred2
Pred2Orig<-(Pred2)*(max(df$DEXfat)-min(df$DEXfat))+min(df$DEXfat)
(Pred2Orig)
testDEXfat2<-(testset[, 2])*(max(df$DEXfat)-min(df$DEXfat))+min(df$DEXfat)
(testDEXfat2)
MSENEural2<-sum((Pred2Orig-testDEXfat2)^2)/18
MSENEural2
#neural network with five hidden nodes
Neural3<-neuralnet(DEXfat~., data=trainset, hidden=5, linear.output=TRUE)
Neural3
plot(Neural3)
Pred3<-compute(Neural3, testset[,1:6])
Pred3$net.result
Pred3Orig<-(Pred3$net.result)*(max(df$DEXfat)-min(df$DEXfat))+min(df$DEXfat)
(Pred3Orig)
testDEXfat3<-(testset[, 2])*(max(df$DEXfat)-min(df$DEXfat))+min(df$DEXfat)
(testDEXfat3)
MSENEural3<-sum((Pred3Orig-testDEXfat3)^2)/18
MSENEural3


#plot of NNR1 model
plot(testDEXfat1, Pred1Orig, xlab = 'Observed DEXfat', ylab = 'Predicted DEXfat', main = 'Neural Network Regression Model (Hidden layer with three nodes)', pch = 16, col="blue", xlim=c(10,65), ylim=c(10,65))
abline(0,1, lwd=2, col='red')

#plot of NNR2 model
plot(testDEXfat2, Pred2Orig, xlab = 'Observed DEXfat', ylab = 'Predicted DEXfat', main = 'Neural Network Regression Model (Hidden layer with four nodes)', pch = 16, col="blue", xlim=c(10,65), ylim=c(10,65))
abline(0,1, lwd=2, col='red')

#plot of NNR3 model
plot(testDEXfat3, Pred3Orig, xlab = 'Observed DEXfat', ylab = 'Predicted DEXfat', main = 'Neural Network Regression Model(Hidden layer with five nodes)', pch = 16, col="blue", xlim=c(10,65), ylim=c(10,65))
abline(0,1, lwd=2, col='red')

