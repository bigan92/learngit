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
library("rpart")
library("caret")

#calculate the accuracy of the fitted model
modelaccuracy<- function(rpred,gender_submission) {
  result_1 <- gender_submission$Survived == rpred
  accuary<-sum(result_1)/length(rpred)
  return(accuary)
}

train=read.csv("train.csv",header = TRUE,stringsAsFactors = FALSE)
test=read.csv("test.csv",header = TRUE,stringsAsFactors = FALSE)
gender_submission=read.csv("gender_submission.csv",header = TRUE,stringsAsFactors = FALSE)
summary(train)
full<-bind_rows(train,test)
train$familyscale=train$SibSp+train$Parch+1

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

##���Կ�����large family �����������������ȼ��߱ȵȼ�����������
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
test2=test[,c(2,4,6,7)]
#View(test2);
sum(is.na.data.frame(test2))
prediction.rfmodel <- predict(rfmodel, test2)

prediction.rfmodel[prediction.rfmodel>=0.5]<-1
prediction.rfmodel[prediction.rfmodel<0.5]<-0
prediction.rfmodel

solution3 <- data.frame(PassengerID = test$PassengerId, Survived_pre= prediction.rfmodel)
full0=bind_cols(gender_submission,solution3)
write.csv(full0,file = "C:\\Users\\bigan\\Documents\\Titanic\\pre.csv")
tab=table(full0$Survived,full0$Survived_pre)
tab
modelaccuracy(prediction.rfmodel,gender_submission)
#(264+144)/nrow(test)

#################
### Make variables factors into factors
factor_variables <- c('PassengerId','Pclass','Sex','Embarked', 'FamilySize')
train[factor_variables] <- lapply(train[factor_variables], function(x) as.factor(x))
train$Survived = as.factor(train$Survived)
test[factor_variables] <- lapply(test[factor_variables], function(x) as.factor(x))



## Build the model#2: Naive Bayes classification - START
naiveBayesModel <- naiveBayes(Survived ~ Pclass + Sex + SibSp + Parch + familyscale, data = train)
summary(naiveBayesModel)
predict.NaiveBayes <- predict(naiveBayesModel,test[,-1])

solution.naiveBayes <- data.frame(PassengerID = test$PassengerId, Survived = predict.NaiveBayes)

## Build the model#2: Naive Bayes classification - END





