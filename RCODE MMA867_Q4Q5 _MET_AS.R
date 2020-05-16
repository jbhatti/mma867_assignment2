install.packages("fpp")
install.packages("forecast")


library(fpp)
library(forecast)
library(lars)
library(Metrics)
library(forcats) 
library(magrittr)
library(MASS)
library(ggplot2)
library(tidyverse)
library(sqldf)
library(gsubfn)
library(RSQLite)
library(glmnet)
library(stringr)
library(dplyr)
library(readxl)
library(mice)
library(fastDummies)
library(car)
library(caret)
library(knitr)
library(ggplot2)
library(zoo)
library(plotly)
library(corrplot)
library(data.table)
library(Rmisc)
library(CRAN)
library(pracma)
library(tseries)




#download from
#https://queensuca.sharepoint.com/:x:/r/teams/GROUP-TeamLawrence/_layouts/15/Doc.aspx?sourcedoc=%7Bf3549031-809a-4b77-9a66-c36a33bbaa0a%7D&action=default&uid=%7BF3549031-809A-4B77-9A66-C36A33BBAA0A%7D&ListItemId=256&ListId=%7B2F6DE06E-80EB-43A0-B7C5-DB642C120188%7D&odsp=1&env=prod


#*********************************************************************************************
##############BE CAREFUL there is 2 tabs##################### PLEASE CHOOSE MET##############
#*********************************************************************************************


#import data
metdata <- read_excel("C:\\Users\\Abi\\Desktop\\Queens\\MMA 867 Predictive Modelling\\Assignment\\EXCEL MMA867_A2_Q4Q5_MET_AS.xlsx", sheet="MetOffice", range= cell_cols(1:3), col_names = TRUE, )

#decompose
metdata_ts <- ts(metdata$Temperature,start=1850, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 
fit <- stl(metdata_ts, t.window=12, s.window="periodic",robust=TRUE) #decompose using STL (Season and trend using Loess)
plot(fit)
plot(metdata_ts )

#ETS
met_aaa <- ets(metdata_ts, model="AAA",damped=FALSE)
met_aan <- ets(metdata_ts, model="AAN", damped=FALSE)
met_aaa

#TBATS with seasonality & trends
#met_tbats <- tbats(metdata_ts)

#create cone
met_aaa_pred <- forecast(met_aaa, h=980, level=c(0.8, 0.95))
met_aan_pred <- forecast(met_aan, h=980, level=c(0.8, 0.95))
#met_tbats_pred <-forecast(met_tbats, h=980, level=c(0.8, 0.95))

#plot
par(mfrow=c(1,3)) # This command sets the plot window to show 1 row of 4 plots
plot(met_aaa_pred, xlab="Year", ylab="Predicted Temp")
plot(met_aan_pred, xlab="Year", ylab="Predicted Temp")
#plot(met_tbats_pred, xlab="Year", ylab="Predicted Temp")
met_aaa_pred


#comparing
f_AAA <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA <- tsCV(metdata_ts, f_AAA, h=1, window=1000)


f_AAN<- function(y, h) forecast(ets(y, model="AAN"), h = h)
errors_AAN <- tsCV(metdata_ts, f_AAN, h=1, window=1000)

#f_TBATS  <- function(y, h) forecast(tbats(y), h = h)
#errors_TBATS <- tsCV(metdata_ts, f_TBATS, h=1, window=1000)

mean(abs(errors_AAA/metdata_ts), na.rm=TRUE)*100
mean(abs(errors_AAN/metdata_ts), na.rm=TRUE)*100
#mean(abs(errors_TBATS/metdata_ts), na.rm=TRUE)*100


#***************Q2****************
#decompose data with predicted anomaly + 14
metdata$Temperature2<-metdata$Temperature+14

#TS
metdata_ts2 <- ts(metdata$Temperature2,start=1850, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 

#ETS
met_aaa2 <- ets(metdata_ts2, model="AAA",damped=FALSE)

#forecast 2030
met_aaa_pred_2030<- forecast(met_aaa2, h=126, level=c(0.9))
#forecast 2050
met_aaa_pred_2050 <- forecast(met_aaa2, h=366, level=c(0.9))
#forecast 2100
met_aaa_pred_2100 <- forecast(met_aaa2, h=976, level=c(0.9))



##*******************************************

#Question 4 &5 MET DATA
train_2013<-subset(metdata, Id<=1956) #separate get pre2013
test_2013<-subset(metdata, Id>= 1957 & Id <=1968) #get 201301 - 201312

train_2013
test_2013

#TS
train_2013_ts <- ts(train_2013$Temperature,start=1850, frequency=12) # ts function defines the dataset as timeseries starting Jan 1850 and having seasonality of frequency 12 (monthly) 
test_2013_ts <- ts(test_2013$Temperature,start=2013, frequency=12) # ts function defines the dataset as timeseries starting Jan 2013 and having seasonality of frequency 12 (monthly) 

#ETS
ets_aaa_2013 <- ets(train_2013_ts, model="AAA",damped=FALSE)


# check to see coefficients Initial states: l = -0.6299 b = -3e-04 
# beta is -3e-04 which is close to zero but not zerO, this means that there is a trend and variability 
 ets_aaa_2013

#the  graph shows variability and higher tempetatures over the years 
plot(ets_aaa_2013) 



 
#create cone
cone_ets_aaa_2013 <- forecast(ets_aaa_2013, h=12, level=c(0.8, 0.95))

plot(cone_ets_aaa_2013)




#forecast 2013
f_AAA_2013  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA_2013 <- tsCV(test_2013_ts, f_AAA_2013, h=1, window=6)

#there is more variability in 2013
#it shows that temp raises compared to prior years so we can't say global warming stopped in 2013
plot(errors_AAA_2013)


#number 5
## lets see temp behaviour after 2013
train2_2013<-subset(metdata, Id<=1956) #separate get pre2013
test2_2013<-subset(metdata, Id>= 1957 & Id <=1968) #get 201301 - 201312

#TS
train2_2013_ts <- ts(train2_2013$Temperature,start=2014, frequency=12) # ts function defines the dataset as timeseries starting Jan 2014 and having seasonality of frequency 12 (monthly) 
test2_2013_ts <- ts(test2_2013$Temperature,start=2013, frequency=12) # ts function defines the dataset as timeseries starting Jan 2013 and having seasonality of frequency 12 (monthly) 

train2_2013_ts
test2_2013_ts

#ETS
ets2_aaa_2013 <- ets(train2_2013_ts, model="AAA",damped=FALSE)

ets2_aaa_2013

#create cone
cone_ets2_aaa_2013 <- forecast(ets2_aaa_2013, h=12, level=c(0.8, 0.95))

#temp keeps increasing even at a faster rate
plot(cone_ets2_aaa_2013)

cone_ets2_aaa_2013


#forecast 2013 again
f2_AAA_2013  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors2_AAA_2013 <- tsCV(test_2013_ts, f2_AAA_2013, h=1, window=6)

#in the second model there is more variability in 2013, we will go ahead to compare post 2013 data to 2013
#it shows that temp raises compared to prior years so we can't say global warming stopped in 2013
plot(errors2_AAA_2013)

errors2_AAA_2013
#Lets see what happens from 2019 onwards

train_2019<-subset(metdata, Id<=2029) #separate get pre2019
test_2019<-subset(metdata, Id>= 2030) #get 201901 - 202002

#TS
train_2019_ts <- ts(train_2019$Temperature,start=2013, frequency=12) # ts function defines the dataset as timeseries starting Jan 2013 and having seasonality of frequency 12 (monthly) 
test_2019_ts <- ts(test_2019$Temperature,start=2019, frequency=12) # ts function defines the dataset as timeseries starting Jan 2019 and having seasonality of frequency 12 (monthly) 

train_2019_ts
test_2019_ts

#ETS
ets_aaa_2019 <- ets(train_2019_ts, model="AAA",damped=FALSE)

#create cone
cone_ets_aaa_2019 <- forecast(ets_aaa_2019, h=12, level=c(0.8, 0.95))

#BETA is 1e-04, alpha=0.4149, gamma = 0.0211 which is also not zero but close, this shows higher variability and less of a stable trend
ets_aaa_2019

#temp keeps increasing even at a faster rate
plot(cone_ets_aaa_2019)


#forecast 2019 
f_AAA_2019  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA_2019 <- tsCV(test_2019_ts, f_AAA_2019, h=1, window=6)

errors_AAA_2019

#Looking at 2019, it looks like the variability is not necessarily large.
#it is similar to the variability in 2013, we will look at this closely on excel
plot(errors_AAA_2019)




write.xlsx(train_2019_ts , "C:\\Users\\Abi\\Desktop\\Queens\\MMA 867 Predictive Modelling\\Assignment.xlsx")


write.xlsx(train_2013_ts , "C:\\Users\\Abi\\Desktop\\Queens\\MMA 867 Predictive Modelling\\Assignment.xlsx")


write.xlsx(test_2013_ts , "C:\\Users\\Abi\\Desktop\\Queens\\MMA 867 Predictive Modelling\\Assignmentyyy.xlsx")


