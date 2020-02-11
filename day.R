#######    ****  PROJECT: BIKE SHARING PREDICTION MODE  **** #############
###   

###################


##    DAY FILE    ##

###################
### PACKAGES THAT WILL BE USED 

install.packages("stringr")
install.packages("fastDummies")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("corrplot")
install.packages("pander")

install.packages("relaimpo")
install.packages("RColorBrewer")

library(MASS)
library(PASWR2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)
library(forecast)
library(zoo)
library(tseries)
library(corrplot)
library(e1071)
library(pander)
library(ggplot2)
library(relaimpo)
library(RColorBrewer)
library(caret)
library(fastDummies)


day_file= read.csv("G:\\DA Project\\day.csv",header = TRUE)
head(day_file)

##############################

###### CSV files summary #####
nrow(day_file)
ncol(day_file)
dim(day_file)
typeof(day_file)
##############################
null_check = is.null(day_file)
null_check

cor(day_file$cnt,day_file$dteday)


# Scatter plot, Visualize the linear relationship between the predictor and response
scatter.smooth(x=day_file$temp, y=day_file$cnt, main="cnt ~ temperature")

##############################
day_file$dteday <- as.Date(day_file$dteday)
day_file$season <- as.factor(day_file$season)
day_file$yr <- as.factor(day_file$yr)
day_file$mnth <- as.factor(day_file$mnth)
day_file$holiday <- as.factor(day_file$holiday)
day_file$weekday <- as.factor(day_file$weekday)
day_file$workingday <- as.factor(day_file$workingday)
day_file$weathersit <- as.factor(day_file$weathersit)
day_file$temp <- as.factor(day_file$temp)
day_file$hum <- as.factor(day_file$hum)
day_file$windspeed <- as.factor(day_file$windspeed)


#day_file$cnt <- as.factor(day_file$cnt)


#DATA PRE-PROCESSING
#correlation on windspeed
cor(day_file$cnt,day_file$windspeed)
#Correlation is weak so drop
cor(day_file$cnt,day_file$weekday)
#Correlation is weak so drop
#multicolleriaty with temp
cor(day_file$temp,day_file$atemp)
#Correlation on humidity
cor(day_file$cnt,day_file$hum)
#correlation on holiday
cor(day_file$cnt,day_file$holiday)
#correlation on weekday
cor(day_file$cnt,day_file$weekday)
#correlation on workingday
cor(day_file$cnt,day_file$workingday)
#correlation on year
cor(day_file$cnt,day_file$yr)
#correlation on month
cor(day_file$cnt,day_file$mnth)
#correlation on season
cor(day_file$cnt,day_file$season)
#correlation on weathersit
cor(day_file$cnt,day_file$weathersit)


#DROPPING
#Instant
day_file$instant=NULL
#windspeed
day_file$windspeed=NULL
#weekday
#day_file$weekday=NULL
## dropped the atemp  attribute
day_file$atemp=NULL
#humidity
day_file$hum=NULL
### drop the dteday variable  as we hve extracted the  month and year 
day_file$dteday=NULL
## drop casual and register as they are leakage  attributes 
day_file$casual=NULL
day_file$registered= NULL
#
day_file$holiday= NULL
#
#day_file$workingday= NULL
#
day_file$weathersit= NULL


head(day_file)


summary(day_file)
summary(day_file)


#correlation on dteday
cor(day_file$cnt,day_file$dteday)



####   *****    Exploratory  Data  Analysis *****  ######################
summary(day_file)


#####Converted categoorical variable
day_file$season <- factor(format(day_file$season, format="%A"),
                          levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))
day_file$weathersit <- factor(format(day_file$weathersit, format="%A"),
                              levels = c("1", "2","3","4") , 
                              labels = c("Sunny","Cloudy/Mist","Rain/Snow/Fog","Heavy Rain/Snow/Fog"))
day_file$holiday <- factor(format(day_file$holiday, format="%A"),
                          levels = c("0","1") , labels = c("Holiday","No-Holiday"))
anv.weather <- anova (day_file)
day_file

#### Boxplot of rental bikes per year#####

ggplot(day_file,aes(yr,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per year") +
  scale_x_discrete(labels = c("2011","2012"))


plot(day_file$dteday,day_file$cnt)
#from the dteday we can infer that we need month and year to analyze the count for two years 
#So its better to drop the dteday and include the year and month in the model


### analyzing the bikes increased in 2012 from 2011


### boxplot of rental bikes based per season#####
#

col <- brewer.pal(4,"Set3")
ggplot(day_file,aes(season,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per season") +
  scale_x_discrete(labels = c("Spring","Summer","Fall","Winter"))
###############################
### the number of bikes rental are increased in summer and fall times

########

col <- brewer.pal(12,"Set3")
ggplot(day_file,aes(mnth,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per month")
##############


#####################################################3
col <- brewer.pal(7,"Set3")
ggplot(day_file,aes(weekday,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by weekday")
## analysed that weekdays are not affecting the count variable so that we can drop that variable


#### analyzing the working days 
ggplot(day_file,aes(workingday,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by workingday") +
  scale_x_discrete(labels = c("no","yes"))

##########


###finding the missing value 
sum(is.na(day_file))

##### To check the exploratory data analysis by normal distribution curve ###### 
## of the data 
## responsible variable  count is distributed 
## here it is normally distributed 
## median here is normally distributed 
## plotting tyhe frquency of rental with total bikes 

#day
h <- hist(day_file$cnt, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'blue' )

xfit <- seq(min(day_file$cnt),max(day_file$cnt), length = 50)
yfit <- dnorm(xfit, mean =mean(day_file$cnt),sd=sd(day_file$cnt))
yfit <- yfit*diff(h$mids[1:2])*length(day_file$cnt)
lines(xfit,yfit, col='red', lwd= 3)

### plotting the box plot for the working day and holiday 
##the medium value is increased for working dayy 
## this means that maximun bike rides are over the working days


boxplot(day_file$cnt ~ day_file$holiday,
        data = day_file,
        main = "Total Bike Rentals Vs Holiday",
        xlab = "Holiday/Working Day",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1", "pink2", "pink3")) 

############################################
## Time series analysis
#needed proper assumptions
data_ts_ <-msts(day_file[,'cnt'], seasonal.periods=c(7))
train_ts_ <- head(data_ts_, round(length(data_ts_) * 0.9))
test_ts_ <- tail(data_ts_, round(length(data_ts_) * 0.1))
plot(train_ts_, xlab="Weeks", ylab="Bike riders")
#day
plot(decompose(train_ts_, type='add'), xlab="Weeks")


##Importing the csv in the R for the regression and EDA
##imported Hours csv file and keep it into 

### two independent samples 

day_file = read.csv("G:\\DA Project\\day.csv", header = TRUE)
head(day_file)
names(day_file)
nrow(day_file)

day_file

##########################################
##########################################
##########################################
#########################################
#n-fold-day
#day
library(tidyverse)

set.seed(123)
training.samples <- day_file$cnt %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data  <- day_file[training.samples, ]
set.seed(123)
train.control <- trainControl(method = "cv", 
                              number = 10)
test.data <- day_file[-training.samples, ]


#######
model <- train(cnt ~  season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed, data = day_file, method = "lm",trControl = train.control)
print(model)
summary(model)
model_2 <- train(cnt ~ season+yr+mnth+holiday+weekday+weathersit+temp+hum+windspeed, data = day_file, method = "lm",trControl = train.control)
print(model_2)
summary(model_2)
model_3 <- train(cnt ~ season+yr+holiday+weekday+weathersit+temp+hum+windspeed, data = day_file, method = "lm",trControl = train.control)
print(model_3)
summary(model_3)
model_4 <- train(cnt ~ season+yr+weekday+weathersit+temp+hum+windspeed, data = day_file, method = "lm",trControl = train.control)
print(model_4)
summary(model_4)

###########################################################
model_4_1 <- lm(cnt ~ season+yr+weekday+weathersit+temp+hum+windspeed, data = day_file)
summary(model_4_1)

#VIF
install.packages("car")
library(car)
library(MASS)
vif(model_4E_1)


##########################################
###########################################
##########################################

#######################################
#AIC-BIC-day_file
head(day_file)
training.samples <- day_file$cnt %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data  <- day_file[training.samples, ]
test.data <- day_file[-training.samples, ]

base_model<-lm(cnt~temp,data=train.data)
summary(base_model)
full_model<-lm(cnt~season+yr+mnth+weekday+workingday+weathersit+temp+hum+windspeed,data=train.data)
summary(full_model)
myaic<-step(base_model,scope=list(upper=full_model,lower=~1),direction="forward",trace=T)
myaic<-step(base_model,scope=list(upper=full_model,lower=~1),direction="both",trace=T)
myaic=step(full_model,direction="backward",trace=T)
print(myaic)
summary(myaic)
nrow(base_model)

#rmse

#COMPUTING RMSE FOR MODEL 50 AND MODEL 10)
#Computing the  RMSE value for MOdel step-AIC 
model_rmse =lm(formula = cnt ~ season + yr + mnth  + weekday + workingday +  weathersit + 
                 temp + hum + windspeed, data = train.data)

y_pred=predict.glm(model_rmse,test.data)
y_obs=test.data[,"cnt"]

model1_rmse= sqrt((y_obs- y_pred)%*%(y_obs - y_pred) /nrow(test.data))
model1_rmse
summary(model1_rmse)

##################################################
##### N-Fold cross validation
set.seed(123)
train.control<- trainControl(method = "cv",number = 10)
model_ridge <- train(cnt~.,data=day_file,method = "lasso",trcontrol= train.control)

print(model_ridge)

############################################################

############################################
#PERFORMING RESIDUAL ANALYSIS
############################################

#STANDARD RESIDUAL VS PREDICTED VALUES

residual=rstandard(model_4_1)
plot(fitted(model_4_1),residual,main="Predicted values V/s the Standard Residual")
abline(a=0,b=0,col='red')

#EXAMINE RESIDUALS ARE NORMAL OR NOT
r<-qqnorm(residual)
r
qqline(residual,col=2)

#USING SHAPIRO WILK NORMALITY TEST
shapiro.test(residual)
#Jarque-Bera
x <- rnorm(r)    # null hypothesis
jarque.bera.test(x)
typeof(myaic)

