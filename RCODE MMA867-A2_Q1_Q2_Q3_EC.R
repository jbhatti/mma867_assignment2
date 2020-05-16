library("Metrics")
library("lars")
library("tidyverse")
library("readxl")
library("sqldf")
library("mice")
library("ggplot2")
library("RSQLite")
library("stringr")
library("dplyr")
library("caret")
library("car")
library("forcats") 
library("magrittr")
library("glmnet")
library("MASS")
library("corrplot")
library(zoo)
library(forecast)
library(fpp)
library("pracma")
library("tseries")
install.packages("tidyr")
#import data
metdata <- read_excel("C:\\Users\\School\\Desktop\\867\\metdata.xlsx")
metdata<-read.csv(file.choose(), header=TRUE, sep=",")
head(metdata)

#Q1
#decompose
metdata_ts <- ts(metdata$Temperature,start=1850, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 
fit <- stl(metdata_ts, t.window=12, s.window="periodic",robust=TRUE) #decompose using STL (Season and trend using Loess)
plot(fit)
plot(metdata_ts )

#ETS
head(metdata_ts)
met_aaa <- ets(metdata_ts, model="AAA",damped=FALSE)
met_aan <- ets(metdata_ts, model="AAN", damped=FALSE)
met_aaa
#TBATS with seasonality & trends
met_tbats <- tbats(metdata_ts)
met_tbats
#create cone
met_aaa_pred <- forecast(met_aaa, h=980, level=c(0.8, 0.95))
met_aan_pred <- forecast(met_aan, h=980, level=c(0.8, 0.95))
met_tbats_pred <-forecast(met_tbats, h=980, level=c(0.8, 0.95))

#plot
par(mfrow=c(1,3)) # This command sets the plot window to show 1 row of 4 plots
plot(met_aaa_pred, xlab="Year", ylab="Predicted Temp")
plot(met_aan_pred, xlab="Year", ylab="Predicted Temp")
plot(met_tbats_pred, xlab="Year", ylab="Predicted Temp")

#comparing
f_AAA <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA <- tsCV(metdata_ts, f_AAA, h=1, window=1000)


f_AAN<- function(y, h) forecast(ets(y, model="AAN"), h = h)
errors_AAN <- tsCV(metdata_ts, f_AAN, h=1, window=1000)
  
f_TBATS  <- function(y, h) forecast(tbats(y), h = h)
errors_TBATS <- tsCV(metdata_ts, f_TBATS, h=1, window=1000)

mean(abs(errors_AAA/metdata_ts), na.rm=TRUE)*100
mean(abs(errors_AAN/metdata_ts), na.rm=TRUE)*100
mean(abs(errors_TBATS/metdata_ts), na.rm=TRUE)*100

# Print the mean and confidence intervals for the MMZ model
met_aaa_pred
met_tbats_pred




# Export the results out
write.csv(met_aaa_pred, file = "Predicted weathe_aaar.csv") # export the selected model's predictions into a CSV file
write.csv(met_tbats_pred, file = "Predicted weather_tbat.csv") # export the selected model's predictions into a CSV file

write.csv(met_aaa_pred,"C:\\drives\\nahid\\Study Materials\\Queens MMA\\867_predictive modelling\\MMM867_A2_Q1_Metoffice_EC.csv", row.names = TRUE)
write.csv(met_tbats_pred,"C:\\drives\\nahid\\Study Materials\\Queens MMA\\867_predictive modelling\\met_tbats_pred_90.csv", row.names = TRUE)

# checking outputs

par(mfrow=c(1,2)) # This command sets the plot window to show 1 row of 4 plots
plot(met_aaa_pred, xlab="Year", ylab="Predicted Temp")
plot(met_tbats_pred, xlab="Year", ylab="Predicted Temp")



#***************Q2****************

#create cone
met_aaa_pred_90 <- forecast(met_aaa, h=980, level=c(0.8, 0.90))
met_aan_pred_90 <- forecast(met_aan, h=980, level=c(0.8, 0.90))
met_tbats_pred_90 <-forecast(met_tbats, h=980, level=c(0.8, 0.90))

write.csv(met_aaa_pred_90,"C:\\drives\\nahid\\Study Materials\\Queens MMA\\867_predictive modelling\\MMM867_A2_Q2_Metoffice_EC.csv", row.names = TRUE)
write.csv(met_tbats_pred_90,"C:\\drives\\nahid\\Study Materials\\Queens MMA\\867_predictive modelling\\met_tbats_pred_90_v2.csv", row.names = TRUE)



#***************Q3****************
#Import
kingston <- read_excel("C:\\Users\\School\\Desktop\\MMA867_Assignment2_Kingston_dataset_2.xlsx")
kingston<-read.csv(file.choose(), header=TRUE, sep=",")
head(kingston)

#transpose kingston data
kingston2<- kingston %>%
  pivot_longer(names_to = "Month",values_to = "Temperature",Jan:Dec)

#decompose
kingston_ts <- ts(kingston2$Temperature,start=1850, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 

#ETS kingston
met_aaa_king <- ets(kingston_ts, model="AAA",damped=FALSE)

#forecast 2030
met_aaa_pred_2030_king<- forecast(met_aaa_king, h=126, level=c(0.9))
#forecast 2050
met_aaa_pred_2050_king <- forecast(met_aaa_king, h=366, level=c(0.9))
#forecast 2100
met_aaa_pred_2100_king <- forecast(met_aaa_king, h=977, level=c(0.9))


write.csv(met_aaa_pred_2030_king,"C:\\drives\\nahid\\Study Materials\\Queens MMA\\867_predictive modelling\\met_aaa_pred_2030_king.csv", row.names = TRUE)
write.csv(met_aaa_pred_2050_king,"C:\\drives\\nahid\\Study Materials\\Queens MMA\\867_predictive modelling\\met_aaa_pred_2050_king.csv", row.names = TRUE)
write.csv(met_aaa_pred_2100_king,"C:\\drives\\nahid\\Study Materials\\Queens MMA\\867_predictive modelling\\MMM867_A2_Q3_Metoffice_EC.csv", row.names = TRUE)

head(met_aaa_pred_2100_king)



