str(train)
str(test)

# To get percentage of people survived 
# prop.table(table(train$Survived))
# 0         1
# 0.6161616 0.3838384

# To add a new column indicating that all died 
# test$Survived<-repo(0,418)


# To submit the answer to kaagle
# submit<-data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
# write.csv(submit,file="theyallperish",row.names = FALSE)


# row.names=False is given such that the output file won't have
# row numbers and such that kaagle recognizes this file

# table(train$Sex)
# prop.table(table(train$Sex,train$Survived),1)

# The second case :
# We have found that for female passengers the proportion of passengers is very high as
# compared to the male passengers.So in the test data set the survived column should be
# 1 for female passengers.
# 
# test$Survived<-0
# test$Survived[test$Sex=='female']<-1

summary(train$Age)
hist(train$Fare)
?hist
library(rpart)

fit<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method = "class")
plot(fit)
text(fit)
fancyRpartPlot(fit)

Prediction<-predict(fit,test,type="class")
submit<-data.frame(PassengerId=test$PassengerId,Survived=Prediction)
write.csv(submit,file = "dtree.csv",row.names = FALSE)

?rpart


fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control( minsplit=2, cp=0))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)


test$Survived<-NA
combi<-rbind(train,test)
strsplit(combi$Name,split='[,.]')

combi$Title<-sapply(combi$Name,FUN=function(x){strsplit(x,split='[,.]')[[1]][2]})
combi$Title
table(combi$Title)
combi$Title<-sub(' ','',combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt','Don','Major','Sir')]<-'Sir'
combi$Title[combi$Title %in% c('Dona','Lady','the Countess','Jonkheer')]<-'Lady'


combi$FamilySize<-combi$SibSp+combi$Parch+1

combi$Surname<-sapply(combi$Name,FUN=function(x){strsplit(x,'[,.]')[[1]][1]})
table(combi$Surname)
combi$FamilyID<-paste(combi$FamilySize,combi$Surname,sep="")

combi$FamilyID[combi$FamilySize<=2]<-'Small'

table(combi$FamilyID[combi$FamilyID=='Small'])
famIds<-data.frame(table(combi$FamilyID))  
famIds<-famIds[famIds$Freq<=2,]


combi$FamilyID[combi$FamilyID %in% famIds$Var1]<-'Small'

combi$FamilyID<-factor(combi$FamilyID)

train<-combi[1:891,]
test<-combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")


