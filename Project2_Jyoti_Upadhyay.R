
load("C:/Users/abc/Project1_Jyoti_Upadhyay.RData")


#############################Project2#########################

#1.Break your data in to random parts train and test, train should contina 70% of the observation. Make
#sure this random sampling is reproducible. [10]

bd$y=as.numeric(bd$y=="yes")

set.seed(2)  #(Setting the seed so that the random sampling is reproducible)

s=sample(1:nrow(bd),0.7*nrow(bd)) #(breaking data into 70% and 30%)

train=bd[s,] #Contaions 70% data
test=bd[-s,] #contains other 30% data

train_lm=train #Assigning train data to train_lm
test_lm=test  #Assigning test data to test_lm

table(train_lm$y)


#2Remove predictor variables with VIF>5 from the train data. [10]

fit=lm(y~.,data=train_lm) #Using linear regressions to identify hich VIFs

#alias(lm(y~.,data=train))

library(car) # loading the car library which consist of vif function

vif(fit) #no variables found with vif value greater than 5



# #3.Build logistic regression model for the response y with remaining variables. Use function step to drop
# insignificant variables from your model. Check if p-values in resulting model for all the variables are
# less than 0.05. If they are not , get rid of such variables sequentially. For thus obtained final model, get
# scores and save them in the train data.[30]

fit_train=glm(y~.,family=binomial,data=train_lm) #Runinng logistic regression on tain

fit=step(fit_train) #running step function on logistic fitted object to remove p values based on AIC

##find the variables and the n running logistic regression on same

formula(fit) 

fit1=glm(y ~ age + balance + duration + campaign + pdays + previous + 
           job_2 + month_1 + month_2 + month_3 + dummy_failure + dummy_other + 
           dummy_unknown + dummy_married + dummy_primary + dummy_secondary + 
           dummy_telephone + housing_dummy + loan_dummy,data=train_lm,family = "binomial")
 
#We may still find out some variables with high p values as step function is based on AIC values

summary(fit1)



#As no further high p value variable is detected

train_lm$score=predict(fit1,newdata=train_lm,type="response") #predicting the probablity scores using predict function




# 4. Get cutoff of the score using KS method. Check performance of the model thus obtained on the test
# data.[20]KS

cutoff_data=data.frame(cutoff=0, KS=99)
cutoffs=seq(0,1,length=1000)
for (cutoff in cutoffs){
  predicted=as.numeric(train_lm$score>cutoff)
  TP=sum(predicted==1 & train_lm$y==1)
  FP=sum(predicted==1 & train_lm$y==0)
  FN=sum(predicted==0 & train_lm$y==1)
  TN=sum(predicted==0 & train_lm$y==0)
  P=TP+FN
  N=TN+FP
  Sn=TP/P
  KS=Sn - (FP/N)
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
}
#remove the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]


cutoff_KS=cutoff_data$cutoff[which.max(cutoff_data$KS)][1] ##getting KS cut off

test_lm$score=predict(fit1,newdata=test_lm,type="response") #predicting the probabolity score on test data

table(test_lm$y,as.numeric(test_lm$score>cutoff_KS))#checking the performance of model over test data

#     0    1
# 0 9604 2312
# 1  286 1362


#Misclassification rate found to be:-(2312+286)/(9604+1362+2312+286)=0.1915364



##################Random Forest#################
#5. Build a random Forest model on the same train data and report if performance of this model is better
# than logistic regression model on the test data. Get a variable importance plot. [30]
train_rf=train
test_rf=test

train_rf$y=as.factor(ifelse(train_rf$y==1,"yes","no"))#storing the response variable in the form of factor

#table(train_rf$y)

test_rf$y=as.factor(ifelse(test_rf$y==1,"yes","no"))

install.packages("randomForest")

library(randomForest)  ##loading the randomforest library
class_rf=randomForest(y~.,data=train_rf) #building random forest model on train data
#class_rf

forest.pred=predict(class_rf,newdata=test_rf)##predicting on test data to check the performance
table(test_rf$y,forest.pred)

#     forest.pred
#       no   yes
# no  11659   257
# yes  1098   550

#Misclassification error found to be :-(257+1098)/(11659+550+257+1098)=0.09989679

names(bd)

#error=(235+1128)/(11681+520+235+1128)

varImpPlot(class_rf) #plotting vriable importnce plot to find the order of importance of the variables

# 6.Pick top6 variables from the variable importance plot and build a logistic regression model with just those
# variables [ get cutoff using KS method]. See how it performs on the test data. [20]

#######Logistic regression with top 6 variables#####

#importance(class_rf)

train_lm_t6=train
test_lm_t6=test

#top 6 variables based on varImpPlot :balance,duration,campaign,pdays,day,age

#Building the logistic model using above 6 variables

fit_train_t6=glm(y ~ balance + duration + campaign + pdays + day+ age
              ,data=train_lm_t6,family="binomial")

fit=step(fit_train_t6) ## Running step function to remove higher p values

formula(fit)

fit=glm(y ~ balance + duration + campaign + pdays + age
        ,data=train_lm_t6,family="binomial")

summary(fit)

train_lm_t6$score=predict(fit,train_lm_t6,type="response")#predictng the probability score on train


#Getting KS cuttoff for the score
cutoff_data=data.frame(cutoff=99,KS=99)
cutoffs=seq(0,1,length=1000)
for( cutoff in cutoffs){
  predicted=as.numeric(train_lm_t6$score>cutoff)
  TP=sum(train_lm_t6$y==predicted & predicted==1)
  FP=sum(train_lm_t6$y!=predicted & predicted==1)
  TN=sum(train_lm_t6$y==predicted & predicted==0)
  FN=sum(train_lm_t6$y!=predicted & predicted==0)
  P=TP+FN
  N=TN+FP
  Sn=TP/P
  Sp=TN/N
  KS=Sn - (FP/N)
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
}
cutoff_data=cutoff_data[-1,]
cutoff_KS=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]

##testing the performance on test data

test_lm_t6$score=predict(fit,test_lm_t6,type="response")

table(test_lm_t6$y,as.numeric(test_lm_t6$score>cutoff_KS))

#Misclassification rate found:- (3545+366)/(8371+1282+3545+366)=0.2883368


##Conclusion:-
#Misclassification rate found from Logistic regression:-(2312+286)/(9604+1362+2312+286)=0.1915364
#Misclassification error found from Random Forest :-(257+1098)/(11659+550+257+1098)=0.09989679
# Misclassification error found from LR using top 6 obtained from Random Forest
#:(3545+366)/(8371+1282+3545+366)=0.2883368

#from above classification error rates obtained it is clear that Random forest is the best suited
#model our problem.

