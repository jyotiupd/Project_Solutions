setwd("E:/Data Science")

getwd()

bd=read.csv("census_income.csv",stringsAsFactors = F)

library(dplyr)

table(bd$race)
table(bd$race_White)

bd=bd %>%
  mutate(race_AIE=as.numeric(race==" Amer-Indian-Eskimo"),
         race_API=as.numeric(race==" Asian-Pac-Islander"),
         race_Black=as.numeric(race==" Black"),
         race_White=as.numeric(race==" White")) %>%
  select(-race)


table(bd$sex_male)

bd=bd %>%
  mutate(sex_male=as.numeric(sex==" Male")
         ) %>%
  select(-sex)



names(bd)

bd$relationship[2]



bd=bd %>% 
  mutate(rel_h=as.numeric(relationship ==" Husband"),
         rel_nif=as.numeric(relationship ==" Not-in-family"),
         relationship_Own_child=as.numeric(relationship ==" Own-child"),
         relationship_Unmarried=as.numeric(relationship ==" Unmarried"),
         relationship_Wife=as.numeric(relationship ==" Wife")) %>% 
  select(-relationship)



round(prop.table(table(bd$workclass,bd$Y),1),1)


bd=bd %>%
  mutate(wc_1=as.numeric(workclass==" Self-emp-inc"),
         wc_2=as.numeric(workclass==" Federal-gov"),
         wc_3=as.numeric(workclass %in% c(" Local-gov"," Self-emp-not-inc"," State-gov")),
         wc_4=as.numeric(workclass==" Private"),
         wc_5=as.numeric(workclass==" ?")) %>%
  select(-workclass)
  
  
  round(prop.table(table(bd$education,bd$Y),1),1)
  
  bd$education[1]
  
  
  bd=bd %>% 
    mutate(education_1=as.numeric(education %in% c(" 10th"," 11th"," 12th"," 7th-8th"," 9th")),
           education_2=as.numeric(education %in% c(" 1st-4th"," 5th-6th"," Preschool")),
          education_3=as.numeric(education %in% c(" Assoc-acdm"," HS-grad"," Some-college")),
          education_4=as.numeric(education ==" Masters"),
          education_5=as.numeric(education ==" Assoc-voc"),
          education_6=as.numeric(education ==" Bachelors")
          ) %>% 
             select(-education)
  
  
  round(prop.table(table(bd$marital.status,bd$Y),1),1)
  
  bd=bd %>% 
    mutate(ms_1=as.numeric(marital.status ==" Never-married"),
           ms_2=as.numeric(marital.status %in% c(" Married-AF-spouse"," Married-civ-spouse"))
           
    ) %>% 
    select(-marital.status)

  round(prop.table(table(bd$occupation,bd$Y),1),1)

  bd=bd %>% 
    mutate(oc_1=as.numeric(occupation==" Exec-managerial"),
           oc_2=as.numeric(occupation==" Prof-specialty"),
           oc_3=as.numeric(occupation %in% c(" Protective-serv"," Sales"," Tech-support")),
           oc_4=as.numeric(occupation %in% c(" Craft-repair"," Transport-moving")),
           oc_5=as.numeric(occupation %in% c(" Priv-house-serv"," Other-service"))) %>%
    select(-occupation)
  
  round(prop.table(table(bd$native.country,bd$Y),1),1)
  
  bd=bd %>%
    mutate(nc_1=as.numeric(native.country %in% c(" Cambodia"," France"," India",
                                                 " Iran"," Japan"," Taiwan"," Yugoslavia")),
           nc_2=as.numeric(native.country %in% c(" ?"," Canada"," China"," Cuba"," England",
                                                 " Germany"," Greece"," Hong"," Italy",
                                                 " Philippines")),
           nc_3=as.numeric(native.country %in% c(" Hungary"," Ireland"," Poland"," Scotland",
                                                 " South"," Thailand"," United-States")),
           nc_4=as.numeric(native.country %in% c(" Columbia"," Dominican-Republic",
                                                 " Guatemala"," Holand-Netherlands",
                                                 " Outlying-US(Guam-USVI-etc)"))) %>%
    select(-native.country)
  
  # this will give % of observations where capital.gain is 0
  sum(bd$capital.gain==0)/nrow(bd)
  
  bd=bd %>%
    mutate(cg_flag0=as.numeric(capital.gain==0))
  
  
  sum(bd$capital.loss==0)/nrow(bd)
  
  bd=bd %>%
    mutate(cl_flag0=as.numeric(capital.loss==0))
  
  
  bd=bd %>%
    mutate(Y=ifelse(Y==" >50K",1,0))
  
  
  table(bd$Y)
  
  load("/Users/lalitsachan/Desktop/March onwards/CBAP with R/Data/temp.Rdata")
  # Above code is to load data from the previous session. If you already have prepared
  # data from data prep session , then you can ignore this safely
  set.seed(123)
  s=sample(1:nrow(bd),0.75*nrow(bd))
  train=bd[s,]
  test=bd[-s,]

  install.packages("car")

  glimpse(bd)
  
  library(car)

  lm_fit=lm(Y~.,data=train)
  sort(vif(lm_fit),decreasing = T)
  
  ?sort()
  
  lm_fit=lm(Y~.-wc_4,data=train)
  
  sort(vif(lm_fit),decreasing = T)
  
  lm_fit=lm(Y~.-wc_4-ms_2,data=train)
  
  sort(vif(lm_fit),decreasing = T)
  
  lm_fit=lm(Y~.-wc_4-ms_2-education_3,data=train)
  
  sort(vif(lm_fit),decreasing = T)
  
  lm_fit=lm(Y~.-wc_4-ms_2-education_3-capital.loss,data=train)
  
  sort(vif(lm_fit),decreasing = T)
  
  
  lm_fit=lm(Y~.-wc_4-ms_2-education_3-capital.loss-race_White,data=train)
  
  sort(vif(lm_fit),decreasing = T)
  
  lm_fit=lm(Y~.-wc_4-ms_2-education_3-capital.loss-race_White-rel_h,data=train)
  
  sort(vif(lm_fit),decreasing = T)
  
  fit=glm(Y~.-wc_4-ms_2-education_3-capital.loss-race_White-rel_h,data=train,family = "binomial")
  
  fit=step(fit)
  
  summary(fit)
  
  
  

  
  
  
