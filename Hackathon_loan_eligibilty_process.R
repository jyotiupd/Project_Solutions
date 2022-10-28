
setwd("E:/Data Science/DataScience/CodeGladiators-ITCinfotech-DataSet")

getwd()

library("dplyr")

train=read.csv("train_data.csv",stringsAsFactors = F)

test=read.csv("test_data.csv",stringsAsFactors = F)

full=bind_rows(train,test)

str(full)

table(full$Loan_Status,full$Gender) ##13 NA

table(full$Married)


##working on dummy varia

full=ful

full=full %>%
  mutate(dummy_male=as.numeric(Gender=="M"),
         dummy_female=as.numeric(Gender=="F")
         )%>%
  select(-Gender)

##Filtering the unknown values from Married

full=full%>%
  filter(Married!="")

full=full %>%
  mutate(dummy_married=as.numeric(Married=="Yes")
  )%>%
  select(-Married)

full=full%>%
  mutate(Dependents=as.numeric(ifelse(Dependents=="3+",3,Dependents)))

prop.table(table(full$Property_Area,full$Loan_Status),2)

full=full%>%
  mutate(dummy_graduate=as.numeric(Education=="Graduate"))%>%
  select(-Education)

full=full%>%
  mutate(dummy_selfEmployed=as.numeric(Self_Employed=="Yes"),
         dumm_notSelfemployed=as.numeric(Self_Employed=="No"))%>%
  select(-Self_Employed)


full=full%>%
  mutate(dummy_urban=as.numeric(Property_Area=="Urban"),
         dummy_Semiurban=as.numeric(Property_Area=="Semiurban"))%>%
  select(-Property_Area)

full=full%>%
  mutate(Loan_Status=as.numeric(Loan_Status=="Y"))

###Imputing NAs

table(is.na(full1$Credit_History))



str(full)

##We'll keep unknown dependents as 0

full=full%>%
  mutate(Dependents=ifelse(is.na(Dependents)==T,0,Dependents))

##Lets remove data of applicants whose requested amount is unknown


full=filter(full,is.na(LoanAmount)==F)


##Replace the NA in loan amount terms with mode of term

full=full%>%
  mutate(Loan_Amount_Term=ifelse(is.na(Loan_Amount_Term)==T,median(Loan_Amount_Term,na.rm=T),Loan_Amount_Term))
full1=full


sapply(full, function(x) sum(is.na(x)))

install.packages("mice")

library(mice)
init = mice(full, maxit=0) 
meth = init$method
predM = init$predictorMatrix

predM[, c("Application_ID")]=0

meth[c("Loan_Status")]=""

meth[c("Credit_History")]="norm" 

set.seed(103)
full1 = mice(full, method=meth, predictorMatrix=predM, m=5)

full1<-complete(full1)

sapply(full1, function(x) sum(is.na(x)))



#After cleansinge the data break it into train and test
train=full1[1:95,]
test=full1[96:590,]

test_lm=test_lm%>%
  select(-Loan_Status)

train_lm=train
test_lm=test

#Remove predictor variables with VIF>5 from the train data.

fit=lm(Loan_Status~.,data=train_lm)

library(car) # loading the car library which consist of vif function

vif(fit) 

fit=lm(Loan_Status~.-dummy_female,data=train_lm)

vif(fit) 

##Building logistic model

fit_train=glm(Loan_Status~.-dummy_female,family=binomial,data=train_lm)

fit=step(fit_train)

formula(fit) 

fit1=glm(Loan_Status ~ CoapplicantIncome + Loan_Amount_Term + Credit_History + 
           dummy_married + dummy_urban + dummy_Semiurban,family=binomial,data=train_lm)

summary(fit1)

fit1=glm(Loan_Status ~   Credit_History + 
             dummy_Semiurban,family=binomial,data=train_lm)


train_lm$score=predict(fit1,newdata=train_lm,type="response") #predicting the probablity scores using predict function

cutoff_data=data.frame(cutoff=0, KS=99)
cutoffs=seq(0,1,length=1000)
for (cutoff in cutoffs){
  predicted=as.numeric(train_lm$score>cutoff)
  TP=sum(predicted==1 & train_lm$Loan_Status==1)
  FP=sum(predicted==1 & train_lm$Loan_Status==0)
  FN=sum(predicted==0 & train_lm$Loan_Status==1)
  TN=sum(predicted==0 & train_lm$Loan_Status==0)
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

table(train_lm$Loan_Status,as.numeric(train_lm$score>cutoff_KS))#checking the performance of model over test data

#     0    1

test_lm=test_lm%>%
  select(-Loan_Status)

test_lm$Loan_Status=as.numeric(test_lm$score>cutoff_KS)


finalouput=test_lm[,c(1,17)]
