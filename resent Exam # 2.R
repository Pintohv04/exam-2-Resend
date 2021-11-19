
# Hugo Pinto  Exam #2

# Question #1 
to_be_predicted <- data.frame(Yesplan4, Bachdeg4,)& (full)
to_be_predicted <- data.frame(Yesplant4,  less than hs ) & (Partime)
to_be_predicted <- data.frame(Yesplant4, GED)
to_be_predicted <- data.frame(Yesplant4,   assoc deg in tech or occ ) & (various)



to_be_predicted$yhat <- predict(Testmodel2, newdata = to_be_predicted2, type = "reponse")
summary(to_be_predicted2$yhat)

to_be_predicted$yhat <- predict(Testmodel2, newdata = to_be_predicted2, type = "response")
summary(to_be_predicted2$yhat)


model_ols <-lm(YES_Parttime_HISPAMNIC+SEX+EDUCA+ INCOME2, data = dat_use)
Summary(model_ols)
tobe_predited2 <- data.frame(HISPANIC= "No", Sex = female, INCOME2, Somecollege
tobe_predited2$ytha <-predict(model_ols, newdata = tobe_predited2, type = "response")
summary(tobe_predited2$ythat  


# Question # 2 

#2a.-2.392

#(-0.019*30+ .00002*(30^2)+-0.470 *0+ 0.0082 *0+ -0.00001*0 - 1.84)

#2b. -2.625
#(-0.019*30+ .00002*(30^2)+-0.470 *1+ 0.0082 *30 *1 + -0.00001*(30^2)*1 - 1.84)

#2c. For females, an older age is two times more likely to get a health insurgence heigter than a male.

 # question # 3

summary(data_NHIS)
use_varb <- (data_NHIS == "Yes")
data.class()
summary(data_NHIS)

use_varb <- (AGE >= 50) & (AGE <=65) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 40) & (DEGFIELD== "Business")
dat_use <- subset(data_NHIS) 
detach()
attach(dat_use)



use_varb <- (AGE >= 20) & (AGE <=60) & (LABFORCE == 2) & (Fimale) (WKSWORK2 > 4) & (HEALTH) (UHRSWORK >= 20) & (DEGFIELD== "Business")
dat_use <- subset(data_NHIS) 
detach()
attach(dat_use)

use_varb <- (AGE >= 20) & (AGE <=50) & (LABFORCE == 2) & (Male) (WKSWORK2 > 4) & (HEALTH) (UHRSWORK >= 20) & (DEGFIELD== "Business")
dat_use <- subset(data_NHIS) 
detach()
attach(dat_use)

use_varb <- (AGE >= 20) & (AGE <=50) & (LABFORCE == 2) & (Male) (WKSWORK2 > 4) & (HEALTH) (UHRSWORK >= 50) & (DEGFIELD== "Business")
dat_use <- subset(data_NHIS) 
detach()
attach(dat_use)


use_varb <- (AGE >= 20) & (AGE <=50) & (LABFORCE == 2) & (Male) (WKSWORK2 > 4) & (Married)  (HEALTH) (UHRSWORK >= 50) & (DEGFIELD== "Business")
dat_use <- subset(data_NHIS) 
detach()
attach(dat_use)


use_varb <- (AGE >= 20) & (AGE <=50) & (LABFORCE == 2) & (Divorse) (Male) (WKSWORK2 > 4) & (Married)  (HEALTH) (UHRSWORK >= 50) & (DEGFIELD== "Business")
dat_use <- subset(data_NHIS) 
detach()
attach(dat_use)


use_varb <- (AGE >= 20) & (AGE <=50) & (LABFORCE == 2) & (single) & (Male) (WKSWORK2 > 4) & (Married)  (HEALTH) (UHRSWORK >= 45) & (DEGFIELD== "Business")
dat_use <- subset(data_NHIS) 
detach()
attach(dat_use)




lm(INCWAGE ~ AGE + I(AGE^2) + female + I(female*Age) + I(female*(AGE^2) + ... ) 
   
   detach()

dat_use(select(Female ,EEDUC, REGION, AGE, INCOME)
model_ols<- (AGE >= 20) & (AGE <= 60) & (LABFORCE == 2) & (WKSWORK2 > 4) & 
(>= 40) & (AfAm== 1) & (female == 1) & (educ_college == 1) 

summary(AGE)

   
        

table(NewDF$HEALTH)

model_ols <-lm(HEALTH +EDUCA+ INCOME,data = dat_use)

model_ols<- (AGE >= 20) & (AGE <= 60) & (White== 2) & (WKFORCE > 4) & 
  (UHRSWORK >= 40) & (AfAm== 1) & (female == 1) & (educ_college == 1) 


model_ols <-lm(YES_HEALTH-X_HISPAMNIC+SEX+EDUCA+ INCOME2, data = dat_use)
Summary(model_ols)
tobe_predited2 <- data.frame(HISPANIC= "No", Sex = female, INCOME2= "less than $15,000 (10,000 less than $15,000)"
tobe_predited2$ytha <-predict(model_ols, newdata = tobe_predited2, type = "response")
summary(tobe_predited2$ythat  


        
        
        
d_educ <- data.frame(model.matrix(~ dat_use1$EEDUC))
summary(d_educ)
levels(dat_use1$EEDUC)

model_ols <-lm(HISPAMNIC+SEX+EDUCA+ INCOME2, data = dat_use)




model_logit <- glm(Excellent_Health$- = SEX +EDUCA, family= binomi)
summary(dat_use)
data_NHIS$YES_HEALTH <- data_NHIS == "yes"

 
summary(data_NHIS)
data_NHIS$YES_Exellent_Health <- data_NHIS$HEALTH_insurence  == "yes"
summary(dat3$YES_Healthinsurece)
use_varb <- (data_NHIS$Divorced  == "Yes")
dat_use <- subset(data_NHIS,use_varb)
summary(dat_use)
model_ols <-lm(YES_ 1-5 years in US- white +SEX+EDUCA+ INCOME2, data = dat_use)
Summary(model_ols)
tobe_predited2 <- data.frame(X_HISPANIC= "No", Sex = female, INCOME2= "less than $15,000 (10,000 less than $15,000)"
tobe_predited2$ytha <-predict(model_ols, newdata = tobe_predited2, type = "response")
summary(tobe_predited2$ythat                                )




summary(data_NHIS)
summary(parttime)
data_NHIS$Excellent_health <- data_NHIS$ == "exellent"

table(data_NHIS$Excellent_Health,dat_use$SEX)
table <- prop.test(table(as.numeric(dat3$Excellent_Health,as.numeric(data_NHIS$SEX)))