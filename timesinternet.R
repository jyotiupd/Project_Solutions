

##Loading the packages
library(dplyr)
library(mice)
library(randomForest)
library(car)



setwd("E:/Data Science/Projects/CodeGladiator-TimesInternet-DataSet")

getwd()

#reading the train data

train=read.csv("train_data.csv",stringsAsFactors = F)

#Adding unique row identifier

train$rid=c(1:nrow(train))

##Setting the NA values of is_fraud to 0

train$is_fraud=ifelse(is.na(train$is_fraud),0,train$is_fraud)

##reading the test data

test=read.csv("test_data.csv",stringsAsFactors = F)

#Adding unique row identifier

test$rid=seq(1048089,2087382,by=1)

#colnames(train)

#Rearranging the columns

train=train%>%
  select(rid,Click.Time,Ad.Slot.Id,Advertiser.Id,Audiences,Call.Identifier,
         Click.IP,User.Id,Geography.Id,Impression.Id,Impression.Ip,
         Ad.id,Reference.Url,Site.Id,User.Agent,is_fraud)

#colnames(test)

#Rearranging the columns


test=test%>%
  select(rid,click_time,ad_slot_id,advertiser_id,audiences,call_identifier,
         click_ip,user_id,geography_id, 
         impression_id,impression_ip,ad_id,reference_url,site_id,user_agent)  

##Creating consitant col names across train and test 

colnames(train)[1:15]=colnames(test)

#colnames(train)=ifelse(is.na(colnames(train))==T,"is_fraud",colnames(train))

##Combining the data into a single data frame
full=bind_rows(train,test)

#Assigning the values
full1=full

#Converting clicck time to numeric format

full1$click_time=as.numeric(as.POSIXct(full1$click_time,format="%Y-%m-%dT%H:%M:%S",tz="Asia/Calcutta"))



##Nothing as such is given as valid or unvalid code for audiences
##so we group the data into known and unknown

full1=full1 %>%
  mutate(audience_known=ifelse(audiences=="",0,1)
  )%>%
  select(-audiences)

re <- regexpr(
  "(?(?=.*?(\\d+\\.\\d+\\.\\d+\\.\\d+).*?)(\\1|))", 
  full1$click_ip, perl = TRUE)

#Greouping the data  into IP and mac address

full1=full1%>%
  mutate(dummy_ip=ifelse(regmatches(click_ip, re)=="",0,1)
  )%>%
  select(-click_ip)

#Grouping the data into yes or no

full1=full1%>%
  mutate(dummy_call_identifier=as.numeric(ifelse(call_identifier=="null",0,1))
  )%>%
  select(-call_identifier)



#sapply(full1, function(x) sum(is.na(x)))


#Replacing the user_agents with their frequency

full1=merge(full1,data.frame(table(user_agent=full$user_agent)),by=c("user_agent"))

full1=full1%>%
  select(-user_agent)


#Rmoving the unimportant data impression ip
full1=full1%>%
  select(-impression_ip)

##.+?[/\s][\d.]+

#breaking the data again in train and test

train1=filter(full1,is.na(is_fraud)==F)

test1=filter(full1,is.na(is_fraud))



##Breaking the train into validation test and train

set.seed(123)

s=sample(1:nrow(train),0.7*nrow(train))

# val_train= train[s,]
# val_test=train[-s,]

val_trainlm=train[s,]
val_testlm=train[-s,]

##First we remove multi collinearity by removing the variables with high vif values one by one

fit_val=lm(is_fraud~.-rid,data=val_trainlm)

vif(fit_val)


##Building a logistic regression model.
fit_val1=glm(is_fraud~.-rid,family="binomial",data=val_trainlm)

##Remove the variables with p values using the step function

fit_val1=step(fit_val1)

formula(fit_val1)

fit_val1=glm(is_fraud ~ click_time + ad_slot_id + advertiser_id + user_id + 
               geography_id + impression_id + ad_id + reference_url + site_id + 
               audience_known + dummy_ip + dummy_call_identifier + Freq  
             ,family="binomial",data=val_trainlm)

summary(fit_val1)


##Predicting the scores on validation train data

val_trainlm$score=predict(fit_val1,newdata = val_trainlm,type = "response")

##Finding out cutt off value through KS

cutoff_data=data.frame(cutoff=0, KS=99)
cutoffs=seq(0,1,length=1000)
for (cutoff in cutoffs){
  predicted=as.numeric(val_trainlm$score>cutoff)
  TP=sum(predicted==1 & val_trainlm$is_fraud==1)
  FP=sum(predicted==1 & val_trainlm$is_fraud==0)
  FN=sum(predicted==0 & val_trainlm$is_fraud==1)
  TN=sum(predicted==0 & val_trainlm$is_fraud==0)
  P=TP+FN
  N=TN+FP
  Sn=TP/P
  KS=Sn - (FP/N)
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
}
#remove the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]

cutoff_KS=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]

##Predicting the scores on validation test data

val_testlm$score=predict(fit_val1,newdata = val_testlm,type = "response")

##Comparing the real and predicted loan status values
table(val_testlm$is_fraud,as.numeric(val_testlm$score>cutoff_KS))

##Missclassification=11615/307958=0.038
##Sensitivity=7607/7815=0.9733845
##Specificity =288736/(288736+11407)=0.9619948


#Predicting on test data

##creating the variable fraud_click_probability with prediction
test1$fraud_click_probability=predict(fit_val1,newdata = test1,type = "response")

##creating the variable is_frud using the cuttoff
test1$is_fraud=test1$fraud_click_probability>cutoff_KS

#merging the datasets so that required columns can be kept

test2=merge(test1,test,by="rid")



solution=test2[,c(1,8)]

write.csv(test,file='solution.csv',row.names=F)

