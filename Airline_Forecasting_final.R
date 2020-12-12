###Building Models on Forecasting
# Forecast the Airlines Passengers data set.
# Prepare a document for each model explaining 
# how many dummy variables you have created and RMSE value for 
# each model. Finally which model you will use for Forecasting.

###SOLUTION :
# Loading required packages
install.packages("forecast")
install.packages("ggplot2")
install.packages("Metrics")
install.packages("fpp") 
install.packages("tseries")
library(Metrics)
library(forecast)
library(ggplot2)
library(fpp)
library(tseries)

# Loading Airline Passenger data
Airline_Data<- read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\Forecasting-R\\Airlines+Data.csv")
View(Airline_Data) # Seasonality 12 months 
hist(Airline_Data$Passengers)
summary(Airline_Data$Passengers)

# The Passenger data does not follow normal distribution.
# However,this will be treated as time series data here onwards

# In order to test the stationarity of the time series, 
# let's run the Augmented Dickey-Fuller Test 
# First set the hypothesis test:
# The null hypothesis H0 : the time series is non stationary 
# The alternative hypothesis HA : the time series is stationary

adf.test(Airline_Data$Passengers)

##  Augmented Dickey-Fuller Test
## Dickey-Fuller = -4.2157, Lag order = 4, p-value = 0.01
## alternative hypothesis: stationary
# As a rule of thumb, where the p-value is less than 5%, 
# we have a strong evidence against the null hypothesis, 
# so we reject the null hypothesis and accept alternate hypothesis 

# Treating data into Time-Series data
passenger<-ts(Airline_Data,frequency = 12)
cycle(passenger)
boxplot(passenger~cycle(passenger), xlab="Passenger Numbers ('000)", ylab="Months", col=rgb(0.1,0.9,0.3,0.4), main="Monthly Air Passengers Boxplot from 1995 to 2002", horizontal=TRUE, notch=FALSE)
#The passenger numbers increase over time with each year which may be
#indicative of an increasing linear trend. Possible due to an 
#increase in demand for flights and commercialisation of airlines 
#in that time period.
#The boxplot shows more passengers travelling in months 6 to 9 with higher averages and higher variances than the other months, indicating seasonality within an apparent cycle of 12 months. The rationale for this could be more people taking holidays and fly over the summer months in the US.
#The dataset appears to be a multiplicative time series, since passenger numbers increase, with a pattern of seasonality.
#There do not appear to be any outliers and there are no missing values.
decompdata<-decompose(passenger,"multiplicative")
plot(decompdata)
# We can distinctly observe the trend, seasonality and overall pattern
# Understanding the pattern w.r.t level, trend and seasonality
plot(Airline_Data$Passengers,type="o")
# The time series pattern indicates the data has a level, upward linear trend,
# and multiplicative seasonality. However, We will confirm by building
# various models

# So creating 11 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X)<-month.abb # Assigning month names 
View(X)
passdata<-cbind(Airline_Data,X)
View(passdata)
passdata["t"]<- 1:96
View(passdata)
passdata["log_pass"]<-log(passdata["Passengers"])
passdata["t_square"]<-passdata["t"]*passdata["t"]
View(passdata)
# Data has been prepared with log, linear "t" and quadratic"t-square"
attach(passdata)
# Splitting data in to Training and Testing
train<-passdata[1:84,]
test<-passdata[85:96,]

########################### LINEAR MODEL #############################
# Building Linear Model 
linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
# R^2 value of the model is 0.79
# Evaluation of model performance
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-rmse(test$Passengers,linear_pred$fit)
rmse_linear
# Accuracy in terms of rmse is 53.2

######################### Exponential #################################
# Building exponential model assuming the exponential trend
expo_model<-lm(log_pass~t,data=train)
summary(expo_model)
# R^2 value of the model has improved to 0.82
# Let's evaluate its performance

# Evaluation of model performance
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
expo_pred
rmse_expo<-rmse(test$Passengers,exp(expo_pred$fit))
rmse_expo 
# Accuracy of the model in terms of rmse is 46.06
######################### Quadratic ####################################
# Building a Quadratic model
Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
# R^2 value is observed to be 0.80

# Evaluation of model performance
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
Quad_pred
rmse_Quad<-rmse(test$Passengers,Quad_pred$fit)
rmse_Quad 
# Accuracy of the model in terms of rmse is 48.05

######################### Additive Seasonality #########################
# Building additive seasonality by considering 11 dummy variables for 11 months
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
# R^2 value is observed to be very low i.e. 0.17

# Evaluation of model performance
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
sea_add_pred
rmse_sea_add<-rmse(test$Passengers,sea_add_pred$fit)
rmse_sea_add
# Accuracy of the model in terms of 132.8

######################## Additive Seasonality with Linear #################
# Building additive seasonality with linear trend
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
# R^2 value is observed to be 0.96

# Evaluation of model performance
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
Add_sea_Linear_pred
rmse_Add_sea_Linear<-rmse(test$Passengers,Add_sea_Linear_pred$fit)
rmse_Add_sea_Linear
# Accuracy of the model in terms of rmse is 35.34
######################## Additive Seasonality with Quadratic #################
# Building additive seasonality with Quadratic trend
Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
# R^2 value is observed to be 0.96

# Evaluation of model performance
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
Add_sea_Quad_pred
rmse_Add_sea_Quad<-rmse(test$Passengers,Add_sea_Quad_pred$fit)
rmse_Add_sea_Quad 
# Accuracy of the model in terms of rmse is 26.36

######################## Multiplicative Seasonality #########################
# Building Multiplicative seasonality
multi_sea_model<-lm(log_pass~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
# R^2 value observed to be low i.e. 0.15

# Evaluation of model performance
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
multi_sea_pred
rmse_multi_sea<-rmse(test$Passengers,exp(multi_sea_pred$fit))
rmse_multi_sea
# Accuracy of the model in terms of rmse is 140.06

######################## Multiplicative Seasonality Linear trend ##########################
# Building model of Multiplicative Seasonality with Linear Trend
multi_add_sea_model<-lm(log_pass~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
# R^2 value is observed to be 0.98

# Evaluation of model performance
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
multi_add_sea_pred
rmse_multi_add_sea<-rmse(test$Passengers,exp(multi_add_sea_pred$fit))
rmse_multi_add_sea 
# Accuracy of the model in terms of rmse is 10.52

## Summary of the models
# Preparing table on model and it's RMSE values 
table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
# From the table,it is understood that Model of multiplicative with
# Linear Trend gives high accuracy and lowest rmse value i.e. 10.5
# Therefore, Multiplicative with linear trend is the suitable model
# Now, we can verify the final model with full dataset

# The Final model is prepared by considering the entire data set.
# Use entire data : Multiplicative seasonality with Linear trend gives least RMSE value
new_model<-lm(log_pass~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = passdata)
new_model_pred<-data.frame(predict(new_model,newdata=passdata,interval='predict'))
new_model_pred

# Getting residuals from the new model
resid <- residuals(new_model)
acf(resid,lag.max = 10)

# By principal of parsimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Auto-regressive model on residuals considering lag-1 
k <- arima(resid, order=c(1,0,0))
pred_res<- predict(arima(resid,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)
plot(k$residuals)
hist(k$residuals)
qqnorm(k$residuals)
qqline(k$residuals)
# The error plot was found to be random.
# Normality and histogram plots of errors show normal distribution
# Therefore, there is no any significant lag in error model

####################### Predicting new data #############################
library(readxl)
test_data<-read_excel(file.choose(),1) #Load Predict_new.xlsx
View(test_data)
#test_data<-Predict_new
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)
pred_new$fit <- pred_new$fit+pred_res$pred
Pred_new_Final< -exp(pred_new)
Pred_new_Final
# This is is the forecasting for the next one year 
# From Jan 2021 to Dec 2021

#CONCLUSION :
# Model of Multiplicative seasonal with linear trend is found to be 
# the best suitable for Airline Passenger dataset.
# Residual model along with main model was found to be the best fit.

