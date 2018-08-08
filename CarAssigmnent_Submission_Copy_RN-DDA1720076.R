#Sankalp Pathak Student Roll Number :DDA1720076 
#Predicctve Analytics - Car Assignment
#
#Selecting working directory

setwd("F:/BA/IIIT Upgrad/Predictive Analytics/CarAssignment")
# Loading required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(plyr)
install.packages("MASS")
library(MASS)
install.packages("corrplot")
library('corrplot') #package corrplot
library('corrplot') #package corrplot
library(car)
list.files(pattern = "csv")
#[1] "CarPrice_Assignment.csv"
#Reading CSV file()
car_price_ori<-read.csv("CarPrice_Assignment.csv",header = T, stringsAsFactors = T)
View(car_price_ori)
str(car_price_ori)


#Data Cleaning

# 1. Selecting unique values apart from car id
car_price_unique<- car_price_ori[!duplicated(car_price_ori [-1]),]
#number of observations in both cases is same. Hence original data frame is used. 


#2. Looking for NA values in data frame 
sum(is.na(car_factr_ori)) # 0

#Data preparation for further analysis

#1. Splitting car name and company name
# before %>% separate(type, c("foo", "bar"), "_and_")

car_price_ori_m<- car_price_ori %>% separate( CarName, c("Company", "Model"), " " )
car_price_ori_m<- car_price_ori_m[,-4]

# car_price_ori_m will be used for further data preparation and modleing. 
View(car_price_ori_m)
str(car_price_ori_m)
summary(car_price_ori_m)
#converting company name from char to factor
car_price_ori_m$Company<-as.factor(car_price_ori_m$Company)
View(car_price_ori_m$Company)
levels(car_price_ori_m$Company)
#Spelling discrepancy in VW, toyota and porsche - Correcting

# revalue(iris$Species, c("virginica" = "setosa")) -> iris$Species
revalue (car_price_ori_m$Company, c("vokswagen" = "vw", "volkswagen" = "vw", 
                                    "porcshce" = "porsche", "Nissan" = "nissan", 
                                    "toyouta" = "toyota", "alfa-romero" = "alfa romeo", 
                                    "maxda" = "mazda") )-> car_price_ori_m$Company
levels(car_price_ori_m$Company) # Verify change in name

View(car_price_ori_m)
str(car_price_ori_m)

#Data preparation:
#1. Company : To assess the perception of american consumer about us car, europian car and asian car
# compnay name factor is distributed in the three levels - "US", "EU" and "AS".

# ["alfa romeo","audi", "bmw","jaguar", "peugeot" , "porsche" , "renault", "saab", "vw","volvo" ]<-"EU"
# ["buick", "chevrolet","dodge", "mercury", "plymouth", "subaru"]<-"US"
# ["honda", "isuzu", "mazda", "mitsubishi", "nissan", "toyota"]<-"AS"
levels(car_price_ori_m$Company) [ 1:3 ]<-"EU"
levels(car_price_ori_m$Company)
#[1] "EU"         "buick"      "chevrolet"  "dodge"      "honda"      "isuzu"      "jaguar"     "mazda"     
#[9] "mercury"    "mitsubishi" "nissan"     "peugeot"    "plymouth"   "porsche"    "renault"    "saab"      
#[17] "subaru"     "toyota"     "vw"         "volvo"     
 levels(car_price_ori_m$Company) [ 2:5]<-"US"
 levels(car_price_ori_m$Company)
#[1] "EU"         "US"         "isuzu"      "jaguar"     "mazda"      "mercury"    "mitsubishi" "nissan"    
# [9] "peugeot"    "plymouth"   "porsche"    "renault"    "saab"       "subaru"     "toyota"     "vw"        
# [17] "volvo"     
levels(car_price_ori_m$Company) [ 3]<-"AS"
levels(car_price_ori_m$Company)
#[1] "EU"         "US"         "AS"         "jaguar"     "mazda"      "mercury"    "mitsubishi" "nissan"    
#[9] "peugeot"    "plymouth"   "porsche"    "renault"    "saab"       "subaru"     "toyota"     "vw"        
#[17] "volvo"     
levels(car_price_ori_m$Company) [ 4]<-"EU"
levels(car_price_ori_m$Company)
# [1] "EU"         "US"         "AS"         "mazda"      "mercury"    "mitsubishi" "nissan"     "peugeot"   
# [9] "plymouth"   "porsche"    "renault"    "saab"       "subaru"     "toyota"     "vw"         "volvo"     
levels(car_price_ori_m$Company) [ 4]<-"AS"
levels(car_price_ori_m$Company)
#[1] "EU"         "US"         "AS"         "mercury"    "mitsubishi" "nissan"     "peugeot"    "plymouth"  
#[9] "porsche"    "renault"    "saab"       "subaru"     "toyota"     "vw"         "volvo"     
levels(car_price_ori_m$Company) [ 4]<-"US"
levels(car_price_ori_m$Company)
#[1] "EU"         "US"         "AS"         "mitsubishi" "nissan"     "peugeot"    "plymouth"   "porsche"   
#[9] "renault"    "saab"       "subaru"     "toyota"     "vw"         "volvo"     
levels(car_price_ori_m$Company) [ 4:5 ]<-"AS"
levels(car_price_ori_m$Company)
#[1] "EU"       "US"       "AS"       "peugeot"  "plymouth" "porsche"  "renault"  "saab"     "subaru"   "toyota"  
# [11] "vw"       "volvo"   
levels(car_price_ori_m$Company) [ 4 ]<-"EU"
levels(car_price_ori_m$Company)
#[1] "EU"       "US"       "AS"       "plymouth" "porsche"  "renault"  "saab"     "subaru"   "toyota"   "vw"      
#[11] "volvo"   
levels(car_price_ori_m$Company) [ 4 ]<-"US"
levels(car_price_ori_m$Company)
#[1] "EU"      "US"      "AS"      "porsche" "renault" "saab"    "subaru"  "toyota"  "vw"      "volvo"  
levels(car_price_ori_m$Company) [ 4:6 ]<-"EU"
levels(car_price_ori_m$Company)
#[1] "EU"     "US"     "AS"     "subaru" "toyota" "vw"     "volvo" 
levels(car_price_ori_m$Company) [ 4]<-"US"
levels(car_price_ori_m$Company)
#[1] "EU"     "US"     "AS"     "toyota" "vw"     "volvo" 
levels(car_price_ori_m$Company) [ 4]<-"AS"
levels(car_price_ori_m$Company)
# [1] "EU"    "US"    "AS"    "vw"    "volvo"
levels(car_price_ori_m$Company) [ 4:5 ]<-"EU"
levels(car_price_ori_m$Company)
#[1] "EU" "US" "AS"

#2. Symboling - converting to three catogories - "safe", "neutral", "risky"
car_price_ori_m$symboling<-as.factor(car_price_ori_m$symboling)
levels(car_price_ori_m$symboling)
# [1] "-2" "-1" "0"  "1"  "2"  "3" 
levels(car_price_ori_m$symboling)[1:2]<- "safe"
levels(car_price_ori_m$symboling)
# [1] "safe" "0"    "1"    "2"    "3"   
levels(car_price_ori_m$symboling)[2]<- "neutral"
levels(car_price_ori_m$symboling)
# [1] "safe"    "neutral" "1"       "2"       "3"   
levels(car_price_ori_m$symboling)[3:5]<- "risky"
levels(car_price_ori_m$symboling)
# [1] "safe"    "neutral" "risky"  


#3 Numerical variables -  Outlier detection in numerical variables and 
# in certain cases Histogram to see whether can be converted to bins 
#3.1 wheelbase
quantile(car_price_ori_m$wheelbase, seq(0,1, 0.01))
# No outlier identified. 
hist(car_price_ori_m$wheelbase)
# Wheel base is a function of type of car hence, bins not created. 
#3.2 car length
quantile(car_price_ori_m$carlength, seq(0,1, 0.01))
hist(car_price_ori_m$carlength)
# No outlier identified.
# Car length is a function of type of car hence, bins not created.
#3.3 car width
quantile(car_price_ori_m$carwidth, seq(0,1, 0.01))
# No outlier identified.
hist(car_price_ori_m$carwidth)
# Car width is a function of car type - bins not created. 
#3.4 Car height 
quantile(car_price_ori_m$carheight, seq(0,1, 0.01))
hist(car_price_ori_m$carheight)
# No outlier identified. Bins not created. 
#3.5 Curb weight 
quantile(car_price_ori_m$curbweight, seq(0,1, 0.01))
hist(car_price_ori_m$curbweight)
# No outlier identified.Bins not created. 
#3.6 Engine size
quantile(car_price_ori_m$enginesize, seq(0,1, 0.01))
hist(car_price_ori_m$enginesize)
# No outlier identified. No bins created. 
#3.7 boreratio 
quantile(car_price_ori_m$boreratio, seq(0,1, 0.01))
hist(car_price_ori_m$boreratio)
# No outlier identified. No bins created.
#3.8 stroke
quantile(car_price_ori_m$stroke, seq(0,1, 0.01))
hist(car_price_ori_m$stroke)
# No outlier identified. No bins created.
#3.9 compression ratio
quantile(car_price_ori_m$compressionratio, seq(0,1, 0.01))
hist(car_price_ori_m$compressionratio)
# No outlier identified. 
# Two bins created
car_price_ori_m$compressionratio[which(car_price_ori_m$compressionratio<12)]<-12
car_price_ori_m$compressionratio[which(car_price_ori_m$compressionratio >  20)]<-20
car_price_ori_m$compressionratio<-as.factor(car_price_ori_m$compressionratio)
levels(car_price_ori_m$compressionratio)
levels(car_price_ori_m$compressionratio)[1]<- "LowComp"
levels(car_price_ori_m$compressionratio)
#[1] "LowComp" "20"     
levels(car_price_ori_m$compressionratio)[2]<- "HighComp"
levels(car_price_ori_m$compressionratio)
# [1] "LowComp"  "HighComp"
#3.10 Horse power
quantile(car_price_ori_m$horsepower, seq(0,1, 0.01))
hist(car_price_ori_m$horsepower)
# No outlier identified. No bins created.
#3.11 Peak rpm
quantile(car_price_ori_m$peakrpm, seq(0,1, 0.01))
hist(car_price_ori_m$peakrpm)
# No outlier identified. No bins created.
#3.12 City mpg
quantile(car_price_ori_m$citympg, seq(0,1, 0.01))
hist(car_price_ori_m$citympg)
# No outlier identified. No bins created.
#3.13 Highway mpg
quantile(car_price_ori_m$highwaympg, seq(0,1, 0.01))
hist(car_price_ori_m$highwaympg)
# No outlier identified. No bins created.
#3.14 Price - Dependant variable 
quantile(car_price_ori_m$price, seq(0,1, 0.01))
hist(car_price_ori_m$price)
# No outlier identified. 


################
#Creating dummy variables for factors
#1. Symboling
dummy_1 <- data.frame(model.matrix(~symboling, data = car_price_ori_m))
dummy_1<-dummy_1[,-1]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m[,-2], dummy_1)
View(car_price_ori_m_1)
#2 . Company
dummy_1 <- data.frame(model.matrix(~Company -1, data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-2], dummy_1)
View(car_price_ori_m_1)
#3 Fuel type
dummy_1 <- data.frame(model.matrix(~fueltype -1, data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-2], dummy_1)
View(car_price_ori_m_1)
# 4. Aspiration
dummy_1 <- data.frame(model.matrix(~aspiration -1 , data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-2], dummy_1)
View(car_price_ori_m_1)
# 5. Door number
dummy_1 <- data.frame(model.matrix(~doornumber -1 , data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-2], dummy_1)
View(car_price_ori_m_1)
# 6. carbody
dummy_1 <- data.frame(model.matrix(~carbody -1 , data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-2], dummy_1)
View(car_price_ori_m_1)
# 7. drivewheel
dummy_1 <- data.frame(model.matrix(~drivewheel -1 , data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-2], dummy_1)
View(car_price_ori_m_1)
# 8. enginelocation
dummy_1 <- data.frame(model.matrix(~enginelocation -1 , data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-2], dummy_1)
View(car_price_ori_m_1)
# 9. enginetype
dummy_1 <- data.frame(model.matrix(~enginetype -1 , data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-7], dummy_1)
View(car_price_ori_m_1)
# 10. cylindernumber
dummy_1 <- data.frame(model.matrix(~cylindernumber -1 , data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-7], dummy_1)
View(car_price_ori_m_1)
# 11. fuelsystem
dummy_1 <- data.frame(model.matrix(~fuelsystem -1 , data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-8], dummy_1)
View(car_price_ori_m_1)
# 12. compressionratio
dummy_1 <- data.frame(model.matrix(~compressionratio -1 , data = car_price_ori_m_1))
dummy_1<-dummy_1[]
View(dummy_1)
car_price_ori_m_1<-cbind(car_price_ori_m_1[,-10], dummy_1)
View(car_price_ori_m_1)

# Derived metrics
# Consumer may like to have power and mileage better or atleast at low power s/he will expect more mileage
# Hence derived metrics is ratio of horse power to mileage. High ratio will indicate increase in mileage with 
# decrease in power. High mileage with high power can be a winner. 
# housing_1$areaperbedroom <- housing_1$area/housing_1$bedrooms
car_price_ori_m_1$hptomileage<-car_price_ori_m_1$highwaympg/car_price_ori_m_1$horsepower
View(car_price_ori_m_1)

# Correlation matrix 
# Correlation matrix 
M <- cor(car_price_ori_m_1) 

corrplot(M, method = "circle") #plot matrix

# We see following parameters are highly corelated with each other : 
# wheelbase to car length : 0.874587475964263
# curbweight to car width : 0.867032464679124
# highwaympg to citympg : 0.971337042342505

# hense can be removed - wheelbase, curbweight, citympg
car_price_ori_m_1<-car_price_ori_m_1[, -c(2, 6, 12)]
View(car_price_ori_m_1)

# Again try candidate for removal 
M <- cor(car_price_ori_m_1)

# None found. 

# Final data frame
# Data preparation completed. Now we store the final data frame as car_price_model for ease of understanding. 
car_price_model<- car_price_ori_m_1
View(car_price_model)

######################333
# Model building 

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(car_price_model), 0.7*nrow(car_price_model))
train = car_price_model[trainindices,]
test = car_price_model[-trainindices,]



# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)


# stepAIC function to remove variables 
step <- stepAIC(model_1, direction="both")

# Building model_2 based on remaining variables
model_2<-lm(formula = price ~ CompanyUS + fuelsystemmfi + carbodyhatchback+ carbodysedan + enginetypeohcf  + 
              peakrpm + carbodyhardtop + doornumberfour + carheight  + drivewheelfwd + highwaympg + hptomileage + 
              enginetypeohcv + carbodyconvertible + CompanyEU + cylindernumbersix  + cylindernumberfive +
              stroke + carwidth + aspirationstd + cylindernumberfour + enginelocationfront + enginesize, 
            data = train)

summary(model_2) 
#VIF for checking multicolinearity 
vif(model_2)

# Building model 3 after removing : highwaympg and hptomileage and cross verify adjusted r square.
model_3<-lm(formula = price ~ CompanyUS + fuelsystemmfi +  carbodysedan + enginetypeohcf  + 
                         peakrpm + carbodyhardtop + doornumberfour + carheight  + drivewheelfwd +  
                             enginetypeohcv + carbodyconvertible + CompanyEU + cylindernumbersix  + cylindernumberfive +
                             stroke + carwidth + aspirationstd + cylindernumberfour + enginelocationfront + enginesize, 
                        data = train)
summary(model_3)
vif(model_3)
# Minor effect on adjusted R squared (from 0.9411 to 0.9392). Hence aceepting model 3 as of now. Next identify furhter candidates
#based on vif and p value. 

# Building model 4 after removing : enginesize, carwidth, cylindernumber4  and cross verify adjusted r square as based on 
# p value they are significant.
model_4<-lm(formula = price ~ CompanyUS + fuelsystemmfi +  carbodysedan + enginetypeohcf  + 
              peakrpm + carbodyhardtop + doornumberfour + carheight  + drivewheelfwd +  
              enginetypeohcv + carbodyconvertible + CompanyEU + cylindernumbersix  + cylindernumberfive +
              stroke +  aspirationstd + enginelocationfront , data = train)
summary(model_4)
vif(model_4)
# Major effect on adjusted R squared (0.9392 to 0.6799). Hence reworking on model 3 and removing non significatn
# variable like fuelsystemmfi,carbodyhardtop,  doornumberfour, drivewheelfwd, carheight
model_5<-lm(formula = price ~ CompanyUS +   carbodysedan + enginetypeohcf  + 
              peakrpm +   enginetypeohcv + carbodyconvertible + CompanyEU + 
              cylindernumbersix  + cylindernumberfive + stroke + carwidth + 
              aspirationstd + cylindernumberfour + enginelocationfront + enginesize, 
            data = train)
summary(model_5)
vif(model_5)
# Minor effect on adjusted R squared (from 0.9392 to 0.9381). Hence aceepting model 5 as of now. Next identify furhter candidates
#based on vif and p value - peakvaluerpm, carbodyconvertible

model_6<-lm(formula = price ~ CompanyUS +   carbodysedan + enginetypeohcf  + 
                 enginetypeohcv +  CompanyEU + 
              cylindernumbersix  + cylindernumberfive + stroke + carwidth + 
              aspirationstd + cylindernumberfour + enginelocationfront + enginesize, 
            data = train)
summary(model_6)
vif(model_6)
# Minor effect on adjusted R squared (from 0.9381 to 0.9347). Hence aceepting model 6 as of now. Next identify furhter candidates
#based on p value - carbodysedan, enginetypeohcv

model_7<-lm(formula = price ~ CompanyUS +   enginetypeohcf  + 
                CompanyEU + cylindernumbersix  + cylindernumberfive + stroke + carwidth + 
              aspirationstd + cylindernumberfour + enginelocationfront + enginesize, 
            data = train)
summary(model_7)
vif(model_7)
#  Minor effect on adjusted R squared (from  0.9347 to 0.9289) and only two variable having two stars (**). One trial after removing 
# CompanyUS and cylindernumberfive
model_8<-lm(formula = price ~ enginetypeohcf  + 
              CompanyEU + cylindernumbersix  +  stroke + carwidth + 
              aspirationstd + cylindernumberfour + enginelocationfront + enginesize, 
            data = train)
summary(model_8)
vif(model_8)
# Minor effect on adjusted R squared (from  0.9289 to 0.9218). REmoving further variable with 
# two stars
# enginetypeohcf, CompanyEU, carwidth
model_9<-lm(formula = price ~ 
               cylindernumbersix  +  stroke + 
              aspirationstd + cylindernumberfour + enginelocationfront + enginesize, 
            data = train)
summary(model_9)
vif(model_9)
# Medium to low effect on adjusted R squared (from  0.9218 to 0.8945). 
# But all variable are significant. 
# Hense will try both models with test data.
#Final selection will base on difference in 

# TEsting model_9
Predict9<-predict(model_9, test[, -11])
test$test_price_9<-Predict9

#Comparing R squared value 
# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price_9)
rsquared <- cor(test$price,test$test_price_9)^2
rsquared

# TEsting model_8
Predict8<-predict(model_8, test[, -11])
test$test_price_8<-Predict8

#Comparing R squared value 
# Now, we need to test the r square between actual and predicted sales. 
r8 <- cor(test$price,test$test_price_8)
rsquared8 <- cor(test$price,test$test_price_8)^2
rsquared8

#Final slected model for pricing of car is model_8.Although difference between R squared is more
# than 5%, however major affecting factors as per model categorical variables and 
# Beta values are negative. Hence, it indicate the proposed absence of certian design parameters 
# like type of engine OHCF. Considering this we take a call to use model_8 for pricing as difference
# is lesser than model_9. 
#Based on Beta values in model_8 we can say that: 
# For better pricing : Higher width, higher engine size is beneficial. 
# Cylinder number shall be different from 4 or 6. 
# Front engine is very high on negative effect on pricing - hence front engine is not a good design 
# for US market to get high price. 
# Also Geely Auto  has to cope up with typical US consumer perception about Europian cars being priecy. 
#So in general to get higher price, Geely Auto  need to work on Brand building, 
# large cars with bigger engine, cylinder numebr more than 4 /6 and downpaly std aspiration. 