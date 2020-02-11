#######    ****  PROJECT: BIKE SHARING PREDICTION MODE  **** #############
###   

###################


##    HOUR FILE    ##

###################
### PACKAGES THAT WILL BE USED 

install.packages("stringr")
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

hour_file= read.csv("G:\\DA Project\\hour.csv",header = TRUE)
head(hour_file)

##############################

###### CSV files summary #####
nrow(hour_file)
ncol(hour_file)
dim(hour_file)
typeof(hour_file)
#view(hour_file)
##############################
null_check = is.null(hour_file)
null_check

summary(hour_file)


# Scatter plot, Visualize the linear relationship between the predictor and response
scatter.smooth(x=hour_file$temp, y=hour_file$cnt, main="cnt ~ temperature")

##############################
hour_file$dteday <- as.Date(hour_file$dteday)
hour_file$season <- as.factor(hour_file$season)
hour_file$yr <- as.factor(hour_file$yr)
hour_file$mnth <- as.factor(hour_file$mnth)
hour_file$holiday <- as.factor(hour_file$holiday)
hour_file$weekday <- as.factor(hour_file$weekday)
hour_file$workingday <- as.factor(hour_file$workingday)
hour_file$weathersit <- as.factor(hour_file$weathersit)
hour_file$temp <- as.factor(hour_file$temp)
hour_file$hum <- as.factor(hour_file$hum)
hour_file$windspeed <- as.factor(hour_file$windspeed)
#hour_file$hr <- as.factor(hour_file$hr)


#hour_file$cnt <- as.factor(hour_file$cnt)

####   *****    Exploratory  Data  Analysis *****  ######################
summary(hour_file)

#DATA PRE-PROCESSING
#correlation on windspeed
cor(hour_file$cnt,hour_file$windspeed)
#Correlation is weak so drop
cor(hour_file$cnt,hour_file$weekday)
#Correlation is weak so drop
#multicolleriaty with temp
cor(hour_file$temp,hour_file$atemp)
#Correlation on humidity
cor(hour_file$cnt,hour_file$hum)
#correlation on holiday
cor(hour_file$cnt,hour_file$holiday)
#correlation on weekday
cor(hour_file$cnt,hour_file$weekday)
#correlation on workingday
cor(hour_file$cnt,hour_file$workingday)
#correlation on year
cor(hour_file$cnt,hour_file$yr)
#correlation on month
cor(hour_file$cnt,hour_file$mnth)
#correlation on season
cor(hour_file$cnt,hour_file$season)
#correlation on dteday
cor(hour_file$cnt,hour_file$hr)


#DROPPING
#Instant
hour_file$instant=NULL
#windspeed
hour_file$windspeed=NULL
#weekday
#hour_file$weekday=NULL
## dropped the atemp  attribute
hour_file$atemp=NULL
#humidity
hour_file$hum=NULL
### drop the dteday variable  as we hve extracted the  month and year 
hour_file$dteday=NULL
## drop casual and register as they are leakage  attributes 
hour_file$casual=NULL
hour_file$registered= NULL
#
hour_file$holiday= NULL
#
hour_file$workingday= NULL
#
hour_file$weathersit= NULL


head(hour_file)

#### Boxplot of rental bikes per year#####

ggplot(hour_file,aes(yr,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per year") +
  scale_x_discrete(labels = c("2011","2012"))

### analyzing the bikes increased in 2012 from 2011


### boxplot of rental bikes based per season#####
#

col <- brewer.pal(4,"Set3")

ggplot(hour_file,aes(season,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per season") +
  scale_x_discrete(labels = c("Spring","Summer","Fall","Winter"))
###############################
### the number of bikes rental are increased in summer and fall times

########
### boxplot of rental bikes based per month#####

col <- brewer.pal(12,"Set3")
ggplot(hour_file,aes(mnth,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per month")

##############
#####################################################3
### boxplot of rental bikes based on weekday#####

col <- brewer.pal(7,"Set3")
ggplot(hour_file,aes(weekday,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by weekday")
## analysed that weekdays are not affecting the count variable so that we can drop that variable
#From this we cannot analyse the varaiance so wee will perform anova on weekday
#PERFORM ANOVA ON WEEKDAY WITH COUNT



head(hour_file)
#### analyzing the working days 
ggplot(hour_file,aes(workingday,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by workingday") +
  scale_x_discrete(labels = c("no","yes"))

##########


###finding the missing value 
sum(is.na(hour_file))

##### To check the exploratory data analysis by normal distribution curve ###### 
## of the data 
## responsible variable  count is distributed 
## here it is normally distributed 
## median here is normally distributed 
## plotting tyhe frquency of rental with total bikes 

#day
h <- hist(hour_file$cnt, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'blue' )

xfit <- seq(min(hour_file$cnt),max(hour_file$cnt), length = 50)
yfit <- dnorm(xfit, mean =mean(hour_file$cnt),sd=sd(hour_file$cnt))
yfit <- yfit*diff(h$mids[1:2])*length(hour_file$cnt)
lines(xfit,yfit, col='red', lwd= 3)
### plotting the box plot for the working day and holiday 
##the medium value is increased for working dayy 
## this means that maximun bike rides are over the working days

#Bocxplot on Holiday
boxplot(hour_file$cnt ~ hour_file$holiday,
        data = hour_file,
        main = "Total Bike Rentals Vs Holiday",
        xlab = "Holiday/Working Day",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1", "pink2", "pink3")) 

############################################
## Time series analysis
#needed proper assumptions
data_ts_ <-msts(hour_file[,'cnt'], seasonal.periods=c(7))
train_ts_ <- head(data_ts_, round(length(data_ts_) * 0.9))
test_ts_ <- tail(data_ts_, round(length(data_ts_) * 0.1))
plot(train_ts_, xlab="Weeks", ylab="Bike riders")
#day
plot(decompose(train_ts_, type='add'), xlab="Weeks")


##Importing the csv in the R for the regression and EDA
##imported Hours csv file and keep it into 

### two independent samples 

hour_file = read.csv("G:\\DA Project\\hour.csv", header = TRUE)
head(hour_file)
names(hour_file)
nrow(hour_file)

##########################################
##########################################
##########################################
#########################################
#n-fold-day
#day
library(tidyverse)


head(hour_file)
set.seed(123)
training.samples <- hour_file$cnt %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data  <- hour_file[training.samples, ]
set.seed(123)
train.control <- trainControl(method = "cv", 
                              number = 10)
test.data <- hour_file[-training.samples, ]

model <- train(log(cnt)~ weekday+hr+season+yr+mnth+temp, data = hour_file, method = "lm",trControl = train.control)
print(model)
summary(model)
model_2 <- train(log(cnt) ~ hr+season+yr+weekday+temp, data = hour_file, method = "lm",trControl = train.control)
print(model_2)
summary(model_2)

my_predictions <- predict(model_2, hour_file, interval="prediction",level=0.95)
summary(my_predictions)

sum<-exp(my_predictions)
summary(sum)

hour_file$cnt1<-exp(log(cnt))

##########################################
###########################################






##########################################

#######################################
#AIC-BIC-hour_file

head(hour_file)
training.samples <- hour_file$cnt %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data  <- hour_file[training.samples, ]
test.data <- hour_file[-training.samples, ]

base_model<-lm(cnt~temp,data=train.data)
summary(base_model)
full_model<-lm(cnt~yr+season+temp+mnth+weekday+hr,data=train.data)
summary(full_model)
myaic<-step(base_model,scope=list(upper=full_model,lower=~1),direction="forward",trace=T)
myaic<-step(base_model,scope=list(upper=full_model,lower=~1),direction="both",trace=T)
myaic=step(full_model,direction="backward",trace=T)
summary(myaic)
############################################################


#GLM


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

#Jarque-Bera
x <- rnorm(r)    # null hypothesis
jarque.bera.test(x)

