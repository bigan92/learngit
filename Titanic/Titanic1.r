setwd('C:\\Users\\bigan\\learngit\\Titanic')
# classification algorithm
library("randomForest")
# visualization
library("ggplot2")
library('ggthemes') 
library('scales')
# data manipulation
library("dplyr")
library('e1071') #Naive Bayes classification
library('mice') # imputation
library("rpart.plot")
library("rpart") #rpart
library("caret")

train=read.csv("train.csv",header = TRUE,stringsAsFactors = FALSE)
test=read.csv("test.csv",header = TRUE,stringsAsFactors = FALSE)
gender_submission=read.csv("gender_submission.csv",header = TRUE,stringsAsFactors = FALSE)
summary(train)
full<-bind_rows(train,test)
train$familyscale=train$SibSp+train$Parch+1

test2=test[,c(2,4,6,7)]
test3=test2
test3$familyscale=test2$SibSp+test2$Parch

##��������
train$Survived <- as.logical(train$Survived)
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
# Show title counts by sex
table(full$Sex, full$Title)
# make explicit factor levels for specific variables: 3=Pclass, 5=Sex, 12=Embarked
for(i in c(3,5,12)) {
  train[,i] <- as.factor(train[,i])
  test[,i-1]<-as.factor(test[,i-1])
}


#̽�������ݷ���  use ggplot2
ggplot(data=train,aes(x=familyscale,y=Pclass,color=Survived))+
  geom_jitter(position = position_jitter(height = .1)) +
  scale_color_manual(values=c("red", "blue")) + facet_grid(Sex ~ .) +
  ggtitle("family scale, Sex, and Class as Survival Factors") + ylab("Pclass")

##���Կ�����large family(>4)�Լ� ������ʿ�����������������ȼ��߱ȵȼ�����������
##Ů�˱����˴�����Ǹ߽ײ�Ů��

#Age ��ȱʧֵ ��ռ��0.2������ɾ�������
(sum(is.na(train$Age))+sum(is.na(test$Age)))/nrow(full)
# + train$Age

rfmodel=randomForest(Survived~Pclass+Sex+ SibSp +Parch, data = train)
rfmodel
vi = varImpPlot(rfmodel)
vi
?varImpPlot
plot(rfmodel)
#View(test2);
sum(is.na.data.frame(test2))
prediction.rfmodel <- predict(rfmodel, test2)

prediction.rfmodel[prediction.rfmodel>=0.5]<-1
prediction.rfmodel[prediction.rfmodel<0.5]<-0
prediction.rfmodel

solution1 <- data.frame(PassengerID = test$PassengerId, Survived= prediction.rfmodel)
write.csv(solution1,file = "RandomForest_prediction.csv",row.names = FALSE)


#calculate the accuracy of the fitted model
modelaccuracy<- function(rpred,gender_submission) {
  result_1 <- gender_submission$Survived == rpred
  accuary<-sum(result_1)/length(rpred)
  return(accuary)
}
#calculate the fitted randomforest model prediction accuracy
modelaccuracy(prediction.rfmodel,gender_submission)
#(264+142)/nrow(test)
###### consider familyscale in the randomforest model
rfmodel2<-randomForest(Survived~Pclass+Sex+ SibSp +Parch+familyscale, data = train)
vi2=varImpPlot(rfmodel2,sort = FALSE)
?varImpPlot
vi2
prediction.rfmodel2 <- predict(rfmodel2, test2)

prediction.rfmodel[prediction.rfmodel>=0.5]<-1
prediction.rfmodel[prediction.rfmodel<0.5]<-0
prediction.rfmodel

solution1 <- data.frame(PassengerID = test$PassengerId, Survived= prediction.rfmodel)
write.csv(solution1,file = "RandomForest_prediction.csv",row.names = FALSE)



## Build the model#2: Naive Bayes classification - START
naiveBayesModel <- naiveBayes(Survived ~ Pclass + Sex + SibSp + Parch + familyscale, data = train)
summary(naiveBayesModel)


predict.NaiveBayes <- predict(naiveBayesModel,as.data.frame(test3))
predict.NaiveBayes[predict.NaiveBayes=="FALSE"]<-'0'
predict.NaiveBayes[predict.NaiveBayes=="TRUE"]<-'1'
levels(predict.NaiveBayes)<-c(0,1)

solution2<- data.frame(PassengerID = test$PassengerId, Survived = predict.NaiveBayes)
write.csv(solution2,"NaiveBayes_prediction.csv",row.names = FALSE)
#modelaccuracy(predict.NaiveBayes,gender_submission)

#how NB model fit the train data
predict_nb_train<-predict(naiveBayesModel,train[,c(3,5,7,8,13)])
modelaccuracy(predict_nb_train,train)
# 0.7620651
## Build the model#2: Naive Bayes classification - END

##build the model#3 rpart --START
str(train)
str(test3)

## rpart.control��������һЩ����  
## xval��10�۽�����֤  
## minsplit����С��֧�ڵ���������ָ���ڵ���20����ô�ýڵ������ֻ���ȥ������ֹͣ  
## minbucket��Ҷ�ӽڵ���С������  
## maxdepth���������  
## cpȫ��Ϊcomplexity parameter��ָĳ����ĸ��Ӷȣ���ÿһ�����,ģ�͵�����Ŷȱ�����ߵĳ̶�  
#ct <- rpart.control(xval=10, minsplit=20, cp=0.1)  
rpart_model<-rpart(Survived ~Pclass + Sex + SibSp + Parch + familyscale, data = train)
#drawn the plot of rpart
rpart.plot(rpart_model)
##also can plot in this way
plot(rpart_model)
text(rpart_model,use.n=T,all=T,cex=0.9)

predict_rpart<-predict(rpart_model,test3,method="exp")
predict_rpart[predict_rpart>=0.5]<-1
predict_rpart[predict_rpart<0.5]<-0
#modelaccuracy(predict_rpart,gender_submission)
#[1] 0.9904306
#how rpart model fit the train data
predict_rp_train<-predict(rpart_model,train[,c(3,5,7,8,13)])
predict_rp_train[predict_rp_train>=0.5]<-1
predict_rp_train[predict_rp_train<0.5]<-0
modelaccuracy(predict_rp_train,train)
# [1] 0.8103255
solution3<- data.frame(PassengerID = test$PassengerId, Survived = predict_rpart)
write.csv(solution3,"rpart_prediction.csv",row.names = FALSE)
##build the model#3 rpart --END

######################
##considering Age use mice package to imputation













