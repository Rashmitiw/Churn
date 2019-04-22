
library(caret)
library(caTools)
library(pROC)
churn<-read.csv("churn.csv",header = T,stringsAsFactors = F,na.strings = "?")
str(churn)


churn$Churn.<-gsub("\\.","",churn$Churn.)
names(churn)<-gsub("\\.","",names(churn))


#checking area code values
table(churn$Area.Code)


#col datatype change

churn$IntlPlan<-factor(churn$IntlPlan)
churn$VMailPlan<-factor(churn$VMailPlan)
churn$Churn<-factor(churn$Churn)
churn$AreaCode<-as.numeric(churn$AreaCode)
churn$EveCalls<-as.numeric(churn$EveCalls)
churn$NightCharge<-as.numeric(churn$NightCharge)
churn$IntlCalls<-as.numeric(churn$IntlCalls)
churn$IntlCharge<-as.numeric(churn$IntlCharge)
churn$DayCharge<-as.numeric(churn$DayCharge)
churn$AreaCode<-factor(churn$AreaCode)
churn$EveMins<-as.numeric(churn$EveMins)


str(churn)

#remove id col from dataset
churn1<-churn[-1] 
str(churn1)



#remove NA

#Knn imputation
library(VIM)
churn2<-kNN(churn1)
summary(churn2)
str(churn2)
churn3<-subset(churn2,select=1:21)
summary(churn3)
str(churn3)

set.seed(12)

churnrf<-churn3[,-c(1,3,4,6,7,8,9,11,14,15,17,19)] #90.91

str(churnrf)
ind<-createDataPartition(churnrf$Churn,p=2/3,list = FALSE)
trainC<-churnrf[ind,]
testC<-churnrf[-ind,]
str(trainC)

Control<-trainControl(method="cv",number=10,savePredictions = TRUE,classProbs = TRUE)





#Random Forest

RF<-train(Churn~.,data=trainC,method="rf",parms = list(split = "information"),
          trControl=Control)
summary(RF)
prediction3<-predict(RF,testC)
PredictionWithProbs3<-predict(RF,testC,type="prob")
auc<-auc(testC$Churn,PredictionWithProbs3[,2])
auc
confusionMatrix(prediction3,testC$Churn) 

