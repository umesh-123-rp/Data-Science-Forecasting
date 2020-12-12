###Building Models on Forecasting
# Forecast the coca cola data set.
# Loading required packages
install.packages(c("forecast","fpp","smooth","tseries"))
library(forecast)
library(fpp)
library(smooth)
library(tseries)

# Loading CocaCola Data
CocaCola_Sales_Rawdata<-read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\Forecasting-R\\CocaCola_Sales_Rawdata.csv")
View(CocaCola_Sales_Rawdata)

# Converting data into time series object
amts<-ts(CocaCola_Sales_Rawdata$Sales,frequency = 4,start=c(86))
View(amts)
summary(amts)
plot(amts)
# The data plotted was showing linear upward trend with multiplicative seasonality

# Further dataset was composed as follows :
decomdata<- decompose(amts, "multiplicative")
plot(decomdata)

# The dataset was decomposed distinctly for trend, seasonality and random

boxplot(amts~cycle(amts,xlab = "Date", ylab = "Cococola sales",
                        main = "Quarterly Sales from Year 1986 to 1996"))
# Overall there is increasing trend of Sales. The sales in Q2 is the highest.

#####MODEL BUILDING PROCESS########
# dividing entire data into training and testing data 
train<-amts[1:38]
test<-amts[39:42] # Considering only 4 Quarters of data for testing
#because data itself is Quarterly seasonal data

# converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)

# Plotting time series data
plot(train) # Visualization shows that it has level, trend, seasonality => Additive seasonality

#### USING HoltWinters function ################
# Optimum values manually by giving input of alpha, beta and gamma

# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)

# Evaluation of performance
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         # By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape
# The error% in terms of MAPE value is observed to be 16.13

# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)

# Evaluation of performance
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 4))
plot(forecast(hw_ab,h=4))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape
# The error % in termss of MAPE is observed to be 8.93

# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)

# Evaluation of performance
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
plot(forecast(hw_abg,h=4))
# by looking at the plot the characters of forecasted values are closely following historical data
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape
# The error % in terms of MAPE is 3.54
########AUTO VALUES OF APLHA, BETA AND GAMMA ###########
# Algorithm can be trained with auto alpha, beta and gamma values
# Prediction model can be built as follows :

# With auto value of alpha, the model was built as follows : 
hw_na<-HoltWinters(train,beta = F,gamma = F)

# Evaluation of performance
hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape
# with auto alpha value of 0.5, MAPE is 9.09%

# Another model is built with auto value of alpha and beta
hw_nab<-HoltWinters(train,gamma=F)

# Evaluation of performance
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape
# With auto value of alpha = 0.57 and beta = 0.31, MAPE is 8.6

# With auto values of alpha, beta and gamma, the model can be built as foolows :
hw_nabg<-HoltWinters(train)

# Evaluation of performance
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape
# With auto value of alpha =0.37, beta=0.25 and gamma=0.89, the MAPE is 2.39%

# SUMMARY of MAPE values for all the models in both manual and auto 
# values of alpha , beta and gamma
df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

# Based on the lowest MAPE values, holts winter exponential model was finalised
# Data level, trend, seasonality characters with default values of alpha, beta and gamma
# The final model selected is hw_nabg<-HoltWinters(train)

# Final Model Building
# The final model is prepared by taking the model with lowest MAPE 
# and to be checked against entire dataset
new_model <- HoltWinters(amts)
plot(forecast(new_model,h=4))

# Forecasted values for the next 4 quarters
forecast_new <- data.frame(forecast(new_model,h=4))
forecast_new

# CONCLUSION :
# Coca cola Sales data set was analysed by using Exponential smoothing 
# Technique i.e. Holtwinters to optimise the final model
# The final prediction for next 4 quarters were done at 80% and 95% cnfidence level

######################################################################




