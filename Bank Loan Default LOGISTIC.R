#==========================
# Read Train and Test CSV
#==========================
train<-read.csv("C:\\Users\\lenovo\\OneDrive\\Documents\\bankloancasestudyb12\\loan_details_train.csv",na.strings = "")
test<-read.csv("C:\\Users\\lenovo\\OneDrive\\Documents\\bankloancasestudyb12\\loan_details_test.csv",na.strings = "")
sample<-read.csv("C:\\Users\\lenovo\\OneDrive\\Documents\\bankloancasestudyb12\\loan_details_sample.csv",na.strings = "")


# Identify number of records and variables
dim(train)
dim(test)
dim(sample)

#Create seprate data sets
train1<-train
test1<-test
sample1<-sample

#Idetify numbers of records and variables
dim(train1)
dim(test1)

#=============================
#Checking Duplicates
#=============================

train1[duplicated(train1$ID),]
View(train1)

#checking number of duplicates rows 
train1[duplicated(train1),]

str(train1)

#===============================
#Assign data types for variables
#===============================
str(train1)
train1$branch_id<-as.factor(train1$branch_id)
train1$MobileNo_Avl_Flag<-as.factor(train1$MobileNo_Avl_Flag)
train$Aadhar_flag<-as.factor(train1$Aadhar_flag)
train1$PAN_flag<-as.factor(train1$PAN_flag)
train$VoterID_flag<-as.factor(train1$VoterID_flag)
train$Driving_flag<-as.factor(train1$Driving_flag)
train$Passport_flag<-as.factor(train1$Passport_flag)
train$default<-as.factor(train1$default)

#==============================
##Transform Date Variable
#==============================
#Convert DOB into proper date format
train1$Date.of.Birth<-as.Date(train1$Date.of.Birth, "%d-%m-%Y")
str(train1)
#Extract age of borrowers in months from DOB
install.packages("eeptools")
library(eeptools)
train1$Age<-(round(age_calc(train1$Date.of.Birth, units = "years")))
str(train1)
class(train1$Age)
train1<-train1[,-(7)]
View(train1)
#Convert DisbursalDate into proper date format
train1$DisbursalDate<-as.Date(train$DisbursalDate, "%d-%m-%Y")

#Extract loan tenure using Disbursal Date
train1$disb_days<- as.numeric(round(age_calc(train1$DisbursalDate, units = "days")))
str(train1)                              
class(train1$disb_days)

#==================================
#Deleting DisbursalDate variable
#==================================
train1<-train1[,-8]
#OR
#train1<-select(train1, -c(DisbursalDate))
View(train1)

#===================================
#Transforming CREDIT,HISTORY.LENGHTH
#===================================

library(dplyr)
library(tidyr)
train1<-train1 %>%separate(CREDIT.HISTORY.LENGTH,c("YEARS","MONTHS"))
str(train1)

#Deleting yrs from Years and mon from Months
train1$YEARS<-gsub("yrs","",train1$YEARS)
train1$MONTHS<-gsub("mon","",train1$MONTHS)

#convert years and months to integer
train1$MONTHS<-as.integer(train1$MONTHS)
train1$YEARS<-as.integer(train1$YEARS)

#Multiply years X 12 to Convert into months
train1$YEARS<-train1$YEARS*12
str(train1)

#Total number of months of CREDIT.HISTORY.LENGTH
train1$CREDIT.HISTORY.LENGTH<-train1$YEARS+train1$MONTHS
View(train1)

#==================================
#Deleting YEARS & MONTHS variable
#==================================
train1<-select(train1, -c(YEARS,MONTHS))

#==================================
#Convert NA to Missing
#==============================
colSums(is.na(train1))
train1$Employment.Type[is.na(train1$Employment.Type)]<- "Missing"
train1$Employment.Type<-as.factor(train1$Employment.Type)
summary(train1$Employment.Type)


###Visualization

plot(log(train1$disbursed_amount),log(train1$asset_cost))
cor(train1$disbursed_amount,train1$asset_cost)
cor(train1$disbursed_amount,train1$ltv)
cor(train1$asset_cost,train1$ltv)

# can drop disburse Amount

boxplot(log(train1$asset_cost)~(train1$branch_id))

# Boxplot with Multivariate variables

par(mfrow=c(2,2))
boxplot(log(train1$asset_cost)~train1$branch_id,subset = train1$region =="North")
boxplot(log(train1$asset_cost)~train1$branch_id,subset = train1$region =="East")
boxplot(log(train1$asset_cost)~train1$branch_id,subset = train1$region =="West")
boxplot(log(train1$asset_cost)~train1$branch_id,subset = train1$region =="South")

dev.off()

#finding relationship between categorical vars using freq. counts
a<-table(train1$Employment.Type,train1$region)
b<-table(train1$default,train1$region)

#Stacked bar chart
barplot(prop.table(a,2),legend=T)
barplot(prop.table(b,2),legend=T)

train1<-train1[,(-8)]
train1<-train1[,(-1)]

## 1st GLM ##
mod<- glm(default~.,data = train1,family = "binomial")
summary(mod)
colnames(train1)
step(mod,data=train1,direction = "both")
library(car)
mod<-glm(formula = default ~ disbursed_amount + asset_cost + ltv + 
      branch_id +Employment.Type + PAN_flag+PERFORM_CNS.SCORE + 
      DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS + NO.OF_INQUIRIES + Age + 
      disb_days + CREDIT.HISTORY.LENGTH, family = "binomial", data = train1)
summary(mod)
vif(mod)


str(train1)
train1$default<-as.factor(train1$default)
train1$region<-as.factor(train1$region)

#================================
#Residual analysis on Model(mod)
#================================
#Will give log of odds
pred1<-predict(mod)#gives logit(hat(p_i))
summary(pred1)


