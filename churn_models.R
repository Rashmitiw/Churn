library(caret)
library(caTools)
library(pROC)
ind<-createDataPartition(churnf$Churn,p=2/3,list = FALSE)
trainC<-churnf[ind,]
testC<-churnf[-ind,]
str(trainC)

Control<-trainControl(method="cv",number=10,savePredictions = TRUE,classProbs = TRUE)

#logistics Regression

LG<-train(Churn~.,data=trainC,method="glm",family="binomial",trControl=Control)
summary(LG)
prediction<-predict(LG,testC)
PredictionWithProbs<-predict(LG,testC,type="prob")
auc<-auc(testC$Churn,PredictionWithProbs[,2])
auc


confusionMatrix(prediction,testC$Churn) 



#Decision Tree

DC<-train(Churn~.,data=trainC,method="rpart",parms = list(split = "information"),
           trControl=Control,
           tuneLength = 10)
plot(DC)
summary(DC)
prediction2<-predict(DC,testC)
PredictionWithProbs2<-predict(DC,testC,type="prob")
auc2<-auc(testC$Churn,PredictionWithProbs2[,2])
auc2
confusionMatrix(prediction2,testC$Churn) 



#Random Forest

RF<-train(Churn~.,data=trainC,method="rf",parms = list(split = "information"),
           trControl=Control)
summary(RF)
prediction3<-predict(RF,testC)
PredictionWithProbs3<-predict(RF,testC,type="prob")
auc3<-auc(testC$Churn,PredictionWithProbs3[,2])
auc3
confusionMatrix(prediction3,testC$Churn) 

#KNN
KN<-train(Churn~.,data=trainC,method="knn",prob=TRUE,use.all=TRUE,
          trControl=Control)
summary(KN)
prediction4<-predict(KN,testC)
PredictionWithProbs4<-predict(KN,testC,type="prob")
auc4<-auc(testC$Churn,PredictionWithProbs4[,2])
auc4

confusionMatrix(prediction4,testC$Churn) 



#svmLinear
set.seed(1677)
SVM<-train(Churn~.,data=trainC,method="svmLinear",
          trControl=Control)
summary(SVM)
prediction5<-predict(SVM,testC)
PredictionWithProbs5<-predict(SVM,testC,type="prob")
auc5<-auc(testC$Churn,PredictionWithProbs5[,2])
auc5

confusionMatrix(prediction5,testC$Churn) 


