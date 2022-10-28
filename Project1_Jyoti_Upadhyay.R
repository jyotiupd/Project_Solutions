##1:-Import File "bank-full.csv" , ensure that categorical variables are imported as characters , not factors.

###a:-Set the work directory to the path which contains the data file
setwd("E:/Projects")

getwd()


##C:- Read the file bank-full using function read.csv ,set the parameter stringsAsFactors = F;
##so that varaiables are imported as characters
bd=read.csv("bank-full.csv",sep=";",stringsAsFactors = F)

########################################################################################################################

#2:-Find out mean ,std deviations, q1 , q3 and IQR for the variables age and balance. Check if they are
#following normal distribution. If they are following normal distribution then calculate outlier limits as :
# mean ± 3*sd, otherwise calculate outlier limits as [q1 - 1.5 * IQR, q3 + 1.5 * IQR]. Find out which
#observations exceed these limits, remove them from the data. [20]

# x=mean(bd$age) #mean of age
# sd1=sd(bd$age) #std. deviation of age
# q1=quantile(bd$age)[2] #q1 of age
# q3=quantile(bd$age)[4] #q3 of age
# 
# IQR(bd$age) #IQR of age36
# 
# 
# x=mean(bd$balance) #mean of balance
# sd1=sd(bd$balance) #std. deviation of balance
# q1=quantile(bd$balance)[2] #q1 of balance
# q3=quantile(bd$balance)[4] #q3 of balance
# 
# IQR(bd$balance) #iqr of balance
# 
# 
# 
# #Normality test
# 
# 
# install.packages("nortest")
# library(nortest)
# ad.test(bd$age)  ##low p value < 2.2e-16 ,distribution is not normal 
# 
# ad.test(bd$balance) ##low p value  < 2.2e-16 ,distribution is not normal 

##Found variables age and balance do not follow normal distribution

#[q1 - 1.5 * IQR, q3 + 1.5 * IQR].

##Function that accepts the variable and returns the upper outlier
# table(is.na(bd$age))
# table(is.na(bd$balance))

outlier_upper=function(x){
  
  return(quantile(x,na.rm = T)[4]+1.5*IQR(x,na.rm = T));
  
}

# quantile(bd$age,na.rm = T)[4]+1.5*IQR(bd$age,na.rm = T)
# quantile(bd$balance,na.rm = T)[4]+1.5*IQR(bd$balance,na.rm = T)

##Function that accepts the variable and returns the lower outlier
outlier_lower=function(x){

  return(quantile(x,na.rm = T)[2]-1.5*IQR(x,na.rm = T));
  
}

# quantile(bd$age,na.rm = T)[2]-1.5*IQR(bd$age,na.rm = T)
# quantile(bd$balance,na.rm = T)[2]-1.5*IQR(bd$balance,na.rm = T)

#table(bd$age)

##Function that accepts the variable and remove both the upper and lower outlier
remove_outlier=function(x)
{
  x=ifelse(x>outlier_upper(x),mean(x),x)
  x=ifelse(x<outlier_lower(x),mean(x),x)
  return(x)
}

bd$age<-remove_outlier(bd$age) #remove the oulier for age

bd$balance<-remove_outlier(bd$balance) #remove the oulier for balance

# min(bd$age)
# max(bd$age)
# table(bd$age)
# table(bd$balance)



###############################################################################################################

#3:-Prepare a percentage cross table for job and y & month and y [separately]. Round of percentages to
#two digist. End Results will look like this. Notice that the percentages for each job category add up to
#one. This shows relative frequency of your resposne across job categories. You need to get similar table
#for months as well. [20]

#round(prop.table(xtabs(~job+y,bd),1),2)

##cross table percentage of job and y susch that
#percentages totat 1 across values of job

# round(prop.table(table(bd$job,bd$y),1),2)
# 
# ##cross table percentage of month and y susch that
# #percentages totat 1 across values of month
# round(prop.table(table(bd$month,bd$y),1),2)


#loading the dplyr package

library(dplyr)

bd=bd %>%
  mutate(job_1=as.numeric(job  %in% c("admin.","self-employed","unknown","management","technician")) ,
         job_2=as.numeric(job == "retired"),
         job_3=as.numeric(job == "unemployed")
         
  ) %>%
  select(-job)



#creating Dummy variable for month



# round(prop.table(table(bd$month,bd$y),1),2)

bd=bd %>%
  mutate(month_1=as.numeric(month %in% c("feb","april"))  ,
         month_2=as.numeric(month =="may"),
         month_3=as.numeric(month %in% c("sep","oct","dec"))
  ) %>%
  select(-month)
###################################################################################################################

#4:-Bonus Question [ it is not mandatory to attempt this, if you do , you get bonus marks ]: Write a
#function which takes a dataset and categorical variable names as input and returns dataset with dummy
#variables for that categorical variable.

createDummy=function(x,z)##function that excepts 2 parameters:-a data set and the categorical variable
{
  ##i=unique(x$z)
  cd=round(prop.table(table(z,x$y),1),2)
  
  dt=as.data.frame.matrix(cd)
  
  #nc=min(dt[,1])
  
  rm=which(dt==min(dt[,1]),arr.ind = T)#find out the rowname of category with lowest probability
  
  #row.names(min(dt[,1]) %in% dt[,1])#removing the rowname of category with lowest probability
  
  rm1=row.names(dt)[-rm[1]]
  
  for(i in unique(rm1))#Iterate through the unique values of categorical variable
  {
    ##print(i)
    x[paste("dummy",i,sep ="_")]<-ifelse(z == i,1,0)##creating the dummy columns for each unique value
    
  }
  
  return(x)
}


#unique(bd$job)

#bd2=createDummy(bd,bd$job)


# debug(createDummy)

#####################################################################################################################################

#5:-Create pi-chart for the variable education with response y as fill


# ggplot(bd, aes(x = education, fill = y)) + geom_bar() + coord_polar(theta = "y")

#####################################################################################################################################

#6:-Using function above [written in Q4] or otherwise create dummy variables for the categorical variables.


#creating dummy variables for categorical variable job,marital,education,housing,loan



bd=createDummy(bd,bd$poutcome)

bd=bd %>%
  select(-poutcome)

bd=createDummy(bd,bd$marital)

bd=bd %>%
  select(-marital)

bd=createDummy(bd,bd$education)

bd=bd %>%
  select(-education)



bd=createDummy(bd,bd$contact)

bd=bd %>%
  select(-contact)


# bd=createDummy(bd,bd$housing)

# bd=bd %>%
#   select(-housing)
# 
# bd=createDummy(bd,bd$loan)
# 
# bd=bd %>%
#   select(-loan)


#round(prop.table(table(bd$housing,bd$y),1),2)



bd=bd %>%
  mutate(housing_dummy=as.numeric(housing=="yes")
         
  ) %>%
  select(-housing)

bd=bd %>%
  mutate(loan_dummy=as.numeric(loan=="yes")
         
  ) %>%
  select(-loan)
# 
# table(bd$y)
# 
# table(bd$default_dummy)

bd=bd %>%
  mutate(default_dummy=as.numeric(default=="yes")
         
  ) %>%
  select(-default)



save.image(file="C:/Users/abc/Project1_Jyoti_Upadhyay.RData")
