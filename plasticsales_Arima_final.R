# Q : ###Building Models on Forecasting
  # Forecast the Plastic Sales data set.
# ARIMA Models  ###########
# Loading required packages
install.packages(c("forecast","fpp","smooth","tseries"))  
library(forecast)
library(fpp)
library(smooth)
library(tseries)

# Converting data into time series object
# Loading dataset
PlasticSales<-read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\Forecasting-R\\PlasticSales.csv")
View(PlasticSales)
summary(PlasticSales$Sales)

# First set the hypothesis test:
# The null hypothesis H0 : that the time series is non stationary 
# The alternative hypothesis HA : that the time series is stationary
adf.test(PlasticSales$Sales)

##  Augmented Dickey-Fuller Test
## Dickey-Fuller = -6.5292, Lag order = 3, p-value = 0.01
## alternative hypothesis: stationary
# As a rule of thumb, where the p-value is less than 5%, 
# we have a strong evidence against the null hypothesis, 
# so we reject the null hypothesis and accept alternate hypothesis.
# So, the time series data of plastic sales is stationary.

# Converting data into time series object
amts<-ts(PlasticSales$Sales,frequency = 12,start=c(49))
View(amts)
plot(amts)
# Sales data indicates linear upward trend with multiplicative seasonal
boxplot(amts~cycle(amts,xlab = "Date", ylab = "Plastic sales",
                   main = "Monthly Sales from Year 1949 to 1953"))
#Overall there is increase trend in Sales.
# The highest salary is in the month of Aug and Sep.
# To understand the trend and seasonality better, decomposition is done on data.
decompdata <-decompose(amts, "multiplicative")
plot(decompdata)

# dividing entire data into training and testing data 
train<-amts[1:48]
test<-amts[49:60] # Considering only Monthly Data

# converting time series object
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)
plot(train)
acf(train)
pacf(train)
# Building Auto.Arima model on Sales data 
library(forecast)
model_AA <- auto.arima(train) 
m<-arima(train,order = c(0,1,0))
model_AA
# Evaluation of performance by checking residuals
acf(model_AA$residuals)
pacf(model_AA$residuals)
plot(model_AA$residuals)
hist(model_AA$residuals)
# The residual ACF and PACF plots indicate that there is no significant lag
# Residual plot and histogram indicates that it follows normal distribution.
# Therefore, the model_AA can be considered as final model.

# We can predict as per the following in graphical way
windows()
plot(forecast(model_AA,h=12),xaxt="n") #xaxt- x axis text 

# CONCLUSION :
# The ARIMA model on this data set has given excellent result.
# So, this model is the best suitable for the plastic sales dataset.



