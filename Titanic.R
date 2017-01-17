##read table
setwd("C:\\Users\\Dongsung\\Desktop\\Kaggle")
tr<-read.csv("train.csv",header=T,,stringsAsFactors = FALSE)
test<-read.csv("test.csv",header=T,,stringsAsFactors = FALSE)
library(gmodels)
library(sqldf)
library(partykit)
library(missForest)
library(randomForest)
library(ggplot2)


## scoring for training set
tr0<-tr
tr0$Age2<-tr0$Age
tr0$Age2<-ifelse(tr0$Age<=10,5,tr0$Age2)
tr0$Age2<-ifelse(tr0$Age>=11&tr0$Age<=20,15,tr0$Age2)
tr0$Age2<-ifelse(tr0$Age>20&tr0$Age<=30,25,tr0$Age2)
tr0$Age2<-ifelse(tr0$Age>30&tr0$Age<=40,35,tr0$Age2)
tr0$Age2<-ifelse(tr0$Age>40&tr0$Age<=50,45,tr0$Age2)
tr0$Age2<-ifelse(tr0$Age>50&tr0$Age<=60,55,tr0$Age2)
tr0$Age2<-ifelse(tr0$Age>60,99,tr0$Age2)




tr0$Fare2<-tr0$Fare
tr0$Fare2<-ifelse(tr0$Fare2<=10,5,tr0$Fare2)
tr0$Fare2<-ifelse(tr0$Fare2>10&tr0$Fare2<=20,15,tr0$Fare2)
tr0$Fare2<-ifelse(tr0$Fare2>20&tr0$Fare2<=30,25,tr0$Fare2)
tr0$Fare2<-ifelse(tr0$Fare2>30&tr0$Fare2<=40,35,tr0$Fare2)
tr0$Fare2<-ifelse(tr0$Fare2>40&tr0$Fare2<=50,45,tr0$Fare2)
tr0$Fare2<-ifelse(tr0$Fare2>50&tr0$Fare2<=60,55,tr0$Fare2)
tr0$Fare2<-ifelse(tr0$Fare2>60&tr0$Fare2<=70,65,tr0$Fare2)
tr0$Fare2<-ifelse(tr0$Fare2>70&tr0$Fare2<=80,75,tr0$Fare2)
tr0$Fare2<-ifelse(tr0$Fare2>80&tr0$Fare2<=90,85,tr0$Fare2)
tr0$Fare2<-ifelse(tr0$Fare2>90&tr0$Fare2<=100,95,tr0$Fare2)
tr0$Fare2<-ifelse(tr0$Fare2>100,100,tr0$Fare2)



tr0$SEXSCORE<-ifelse(tr0$Sex=="female",0.352,0.648)
tr0$PCLASSSCORE<-0
tr0$PCLASSSCORE<-ifelse(tr0$Pclass==1,3,tr0$PCLASSSCORE)
tr0$PCLASSSCORE<-ifelse(tr0$Pclass==2,2,tr0$PCLASSSCORE)
tr0$PCLASSSCORE<-ifelse(tr0$Pclass==3,1,tr0$PCLASSSCORE)
tr0$PARCHSCORE<-0
tr0$PARCHSCORE<-ifelse(tr0$Parch==0,0.761,tr0$PARCHSCORE)
tr0$PARCHSCORE<-ifelse(tr0$Parch==1,0.132,tr0$PARCHSCORE)
tr0$PARCHSCORE<-ifelse(tr0$Parch==2,0.090,tr0$PARCHSCORE)
tr0$EMBARKSCORE<-0
tr0$EMBARKSCORE<-ifelse(tr0$Embarked=="S",0.723,tr0$EMBARKSCORE)
tr0$EMBARKSCORE<-ifelse(tr0$Embarked=="Q",0.086,tr0$EMBARKSCORE)
tr0$EMBARKSCORE<-ifelse(tr0$Embarked=="C",0.189,tr0$EMBARKSCORE)
tr0$AGESCORE<-0
tr0$AGESCORE<-ifelse(tr0$Age2==5,0.090,tr0$AGESCORE)
tr0$AGESCORE<-ifelse(tr0$Age2==15,0.161,tr0$AGESCORE)
tr0$AGESCORE<-ifelse(tr0$Age2==25,0.322,tr0$AGESCORE)
tr0$AGESCORE<-ifelse(tr0$Age2==35,0.217,tr0$AGESCORE)
tr0$AGESCORE<-ifelse(tr0$Age2==45,0.120,tr0$AGESCORE)
tr0$AGESCORE<-ifelse(tr0$Age2==55,0.059,tr0$AGESCORE)
tr0$AGESCORE<-ifelse(tr0$Age2==99,0.031,tr0$AGESCORE)
tr0$FARESCORE<-tr0$FARESCORE



##sampling for traing 
tr1<-tr0[order(tr0$Survived),]
samp<-c(sample(1:549,300),sample(550:891,300))
samp0<-append(samp[1:200],samp[301:500])
samp1<-append(samp[201:300],samp[501:600])

fds.Tr<-tr1[samp0,]
fds.Te<-tr1[samp1,]




x<-subset(fds.Te,select=-Survived)
y<-fds.Te$Survived



rf1<-randomForest(Survived~ SEXSCORE+PCLASSSCORE+PARCHSCORE+EMBARKSCORE,data=fds.Tr, ntree=400, proximity=TRUE,keep.forest=TRUE)
pred1<-predict(rf1,x)
pred1<-ifelse(pred1<0.5,0,1)
table(pred1,y)





##scoring for test set
test$Age2<-test$Age
test$Age2<-ifelse(test$Age<=10,5,test$Age2)
test$Age2<-ifelse(test$Age>=11&test$Age<=20,15,test$Age2)
test$Age2<-ifelse(test$Age>20&test$Age<=30,25,test$Age2)
test$Age2<-ifelse(test$Age>30&test$Age<=40,35,test$Age2)
test$Age2<-ifelse(test$Age>40&test$Age<=50,45,test$Age2)
test$Age2<-ifelse(test$Age>50&test$Age<=60,55,test$Age2)
test$Age2<-ifelse(test$Age>60,99,test$Age2)




test$Fare2<-test$Fare
test$Fare2<-ifelse(test$Fare2<=10,5,test$Fare2)
test$Fare2<-ifelse(test$Fare2>10&test$Fare2<=20,15,test$Fare2)
test$Fare2<-ifelse(test$Fare2>20&test$Fare2<=30,25,test$Fare2)
test$Fare2<-ifelse(test$Fare2>30&test$Fare2<=40,35,test$Fare2)
test$Fare2<-ifelse(test$Fare2>40&test$Fare2<=50,45,test$Fare2)
test$Fare2<-ifelse(test$Fare2>50&test$Fare2<=60,55,test$Fare2)
test$Fare2<-ifelse(test$Fare2>60&test$Fare2<=70,65,test$Fare2)
test$Fare2<-ifelse(test$Fare2>70&test$Fare2<=80,75,test$Fare2)
test$Fare2<-ifelse(test$Fare2>80&test$Fare2<=90,85,test$Fare2)
test$Fare2<-ifelse(test$Fare2>90&test$Fare2<=100,95,test$Fare2)
test$Fare2<-ifelse(test$Fare2>100,100,test$Fare2)


test$SEXSCORE<-ifelse(test$Sex=="female",0.352,0.648)
test$PCLASSSCORE<-0
test$PCLASSSCORE<-ifelse(test$Pclass==1,3,test$PCLASSSCORE)
test$PCLASSSCORE<-ifelse(test$Pclass==2,2,test$PCLASSSCORE)
test$PCLASSSCORE<-ifelse(test$Pclass==3,1,test$PCLASSSCORE)
test$PARCHSCORE<-0
test$PARCHSCORE<-ifelse(test$Parch==0,0.761,test$PARCHSCORE)
test$PARCHSCORE<-ifelse(test$Parch==1,0.132,test$PARCHSCORE)
test$PARCHSCORE<-ifelse(test$Parch==2,0.090,test$PARCHSCORE)
test$EMBARKSCORE<-0
test$EMBARKSCORE<-ifelse(test$Embarked=="S",0.723,test$EMBARKSCORE)
test$EMBARKSCORE<-ifelse(test$Embarked=="Q",0.086,test$EMBARKSCORE)
test$EMBARKSCORE<-ifelse(test$Embarked=="C",0.189,test$EMBARKSCORE)
test$AGESCORE<-0
test$AGESCORE<-ifelse(test$Age2==5,0.090,test$AGESCORE)
test$AGESCORE<-ifelse(test$Age2==15,0.161,test$AGESCORE)
test$AGESCORE<-ifelse(test$Age2==25,0.322,test$AGESCORE)
test$AGESCORE<-ifelse(test$Age2==35,0.217,test$AGESCORE)
test$AGESCORE<-ifelse(test$Age2==45,0.120,test$AGESCORE)
test$AGESCORE<-ifelse(test$Age2==55,0.059,test$AGESCORE)
test$AGESCORE<-ifelse(test$Age2==99,0.031,test$AGESCORE)
test$FARESCORE<-test$FARESCORE

pred<-predict(rf1,test)
pred<-ifelse(pred<0.5,0,1)

submission<-data.frame(PassengerId=test$PassengerId)
submission$Survived<-pred
write.csv(submission,"rf_submission.csv",row.names=F)

imp<-importance(rf1)


featureImp<-data.frame(Feature=row.names(imp),Importance=imp[,1])
p<-ggplot(featureImp, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))


ggsave("feature_importance.png",p)



