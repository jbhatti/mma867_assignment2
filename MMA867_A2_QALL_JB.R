library(tidyr)
library(readxl)
library(sqldf)
library(forecast)
library("weathermetrics")


################################################################################
##########################  This script uses     ###############################
##########################  NASA data only       ###############################
##########################  Tab 2                ###############################
################################################################################



nasa <- read_excel("C:\\Users\\Windows\\Downloads\\MMA867_Assignment2_dataset_Jaison.xlsx", 2)


nasa_clean <- gather(nasa, "month", "temp", -1)



nasa_order <- sqldf("select * from nasa_clean order by Year, month")

nasa_order$month_no <- ifelse(nasa_order$month == 'Jan', 1, 
                              ifelse(nasa_order$month == 'Feb', 2,
                                     ifelse(nasa_order$month == 'Mar', 3,
                                            ifelse(nasa_order$month == 'Apr', 4,
                                                   ifelse(nasa_order$month == 'May', 5,
                                                          ifelse(nasa_order$month == 'Jun', 6,
                                                                 ifelse(nasa_order$month == 'Jul', 7,
                                                                        ifelse(nasa_order$month == 'Aug', 8,
                                                                               ifelse(nasa_order$month == 'Sep', 9,
                                                                                      ifelse(nasa_order$month == 'Oct', 10,
                                                                                             ifelse(nasa_order$month == 'Nov', 11, 12)
                                                                                             ))))))))))

nasa_order <- sqldf("select * from nasa_order order by Year, month_no")

nasa_order$ID <- seq.int(nrow(nasa_order))

nasa_order$YearMonth <- paste(nasa_order$Year, "-" ,nasa_order$month_no)

nasa_order <- sqldf("select * from nasa_order where ID < 1684 order by Year, month_no")

nasa_order$tempK <- celsius.to.kelvin(nasa_order$temp)

plot.ts(nasa_order$tempK, xlab="Year", ylab="Anomaly Temperature (in K)")



nasa_order1 <- sqldf("select ID, Year, month_no, tempK from nasa_order where ID < 1684 order by Year, month_no")
test_ds <- ts(nasa_order1$tempK, start=1880, frequency = 12)

fit <- decompose(test_ds, type='multiplicative')
plot(fit)

fit <- decompose(test_ds, type='additive')
plot(fit)

fit <- stl(test_ds, t.window=12, s.window='periodic', robust=TRUE)
plot(fit)

plot(test_ds, xlab="Year", ylab="Anomaly Temperature (in K)")

library(forecast)

# Create exponential smoothing models: additive vs multiplicative noise (first A vs M), additive vs multiplicative trend (second A vs M) and no seasonality vs automatic detection (third N vs Z) trend and no seasonlity (AAN), multiplicative (MMN)
test_ds_AAN <- ets(test_ds, model="AAN", damped=FALSE)
test_ds_AAA <- ets(test_ds, model="AAA", damped=FALSE)


# Create their prediction "cones" for 960 months (80 years) into the future with quintile confidence intervals
test_ds_AAN_pred <- forecast(test_ds_AAN, h=960, level=c(0.8, 0.95))
test_ds_AAA_pred <- forecast(test_ds_AAA, h=960, level=c(0.9))
test_ds_AAA_pred

# Compare the prediction "cones" visually
par(mfrow=c(1,2)) # This command sets the plot window to show 1 row of 4 plots
plot(test_ds_AAN_pred, xlab="Year", ylab="Temperature")
plot(test_ds_AAA_pred, xlab="Year", ylab="Temperature")


# Lets look at what our models actually are -- ETS
test_ds_AAN
test_ds_AAA


test_ds_AAA2 <- forecast::holt(test_ds, h=960)
test_ds_AAA2
plot(test_ds_AAA2)

#Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model
test_ds_tbats <- tbats(test_ds)
test_ds_tbats_pred <-forecast(test_ds_tbats, h=960, level=c(0.8, 0.95))
par(mfrow=c(1,1))
plot(test_ds_tbats_pred, xlab="Year", ylab="Temperature")

par(mfrow=c(1,3)) # Lets look at the three models with seasonality on one graph on the same scale
plot(test_ds_AAA_pred, xlab="Year", ylab="Temperature", ylim=c(30,40))
plot(test_ds_AAN_pred, xlab="Year", ylab="Temperature", ylim=c(30,40))
plot(test_ds_tbats_pred, xlab="Year", ylab="Temperature", ylim=c(30,40))

# Lets look at what our models actually are -- TBATS
test_ds_tbats

###
### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout)
###

f_AAN  <- function(y, h) forecast(ets(y, model="AAN"), h = h)
errors_AAN <- tsCV(test_ds, f_AAN, h=1, window=960)

f_AAA  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA <- tsCV(test_ds, f_AAA, h=1, window=960)

par(mfrow=c(1,1)) 
plot(errors_AAN, ylab='tsCV errors')
abline(0,0)
lines(errors_AAA, col="green")
legend("bottom", legend=c("CV_error_AAN", "CV_error_AAA"), col=c("black", "green", ), lty=4:1)

mean(abs(errors_AAN/test_ds), na.rm=TRUE)*100
mean(abs(errors_AAA/test_ds), na.rm=TRUE)*100


f_TBATS  <- function(y, h) forecast(tbats(y), h = h)
errors_TBATS <- tsCV(test_ds, f_TBATS, h=1, window=960)

plot(errors_AAA, ylab='tsCV errors', col="green")
abline(0,0)
lines(errors_AAN, col="blue")
lines(errors_TBATS, col="gray")
legend("left", legend=c("CV_error_AAA", "CV_error_AAN","CV_error_TBATS"), col=c("green", "blue", "gray"), lty=1:4)

mean(abs(errors_TBATS/test_ds), na.rm=TRUE)*100

# Print the mean and confidence intervals for the AAA model
test_ds_AAA_pred

################################################################################
##########################                       ###############################
##########################       QUESTION 1&2    ###############################
##########################                       ###############################
################################################################################


# Export the results out
write.csv(test_ds_AAA_pred, file = "Temperatures1.csv") # export the selected model's predictions into a CSV file



################################################################################
##########################                       ###############################
##########################       QUESTION 3      ###############################
##########################                       ###############################
################################################################################


kingston <- read_excel("C:\\Users\\Windows\\Downloads\\MMA867_Assignment2_Kingston_dataset_Jaison.xlsx", 2)

kingston_clean <- gather(kingston, "month", "temp", -1)

kingston_order <- sqldf("select * from kingston_clean order by Year, month")

kingston_order$month_no <- ifelse(kingston_order$month == 'Jan', 1, 
                              ifelse(kingston_order$month == 'Feb', 2,
                                     ifelse(kingston_order$month == 'Mar', 3,
                                            ifelse(kingston_order$month == 'Apr', 4,
                                                   ifelse(kingston_order$month == 'May', 5,
                                                          ifelse(kingston_order$month == 'Jun', 6,
                                                                 ifelse(kingston_order$month == 'Jul', 7,
                                                                        ifelse(kingston_order$month == 'Aug', 8,
                                                                               ifelse(kingston_order$month == 'Sep', 9,
                                                                                      ifelse(kingston_order$month == 'Oct', 10,
                                                                                             ifelse(kingston_order$month == 'Nov', 11, 12)
                                                                                      ))))))))))

kingston_order <- sqldf("select * from kingston_order order by Year, month_no")

kingston_order$ID <- seq.int(nrow(kingston_order))

kingston_order$YearMonth <- paste(kingston_order$Year, "-" ,kingston_order$month_no)

kingston_order$tempK <- celsius.to.kelvin(kingston_order$temp)

par(mfrow=c(1,1)) 
plot.ts(kingston_order$tempK, xlab="Year", ylab="Anomaly Temperature (in K)")

test_ds_kingston <- ts(kingston_order$tempK, start=1850, frequency = 12)

fit <- stl(test_ds_kingston, t.window=12, s.window='periodic', robust=TRUE)
plot(fit)

plot(test_ds_kingston, xlab="Year", ylab="Anomaly Temperature (in K)")


library(forecast)

# Create exponential smoothing models: additive vs multiplicative noise (first A vs M), additive vs multiplicative trend (second A vs M) and no seasonality vs automatic detection (third N vs Z) trend and no seasonlity (AAN), multiplicative (MMN)
test_ds_AAA <- ets(test_ds_kingston, model="AAA", damped=FALSE)


# Create their prediction "cones" for 960 months (80 years) into the future with quintile confidence intervals
test_ds_AAA_pred <- forecast(test_ds_AAA, h=960, level=c(0.9))
test_ds_AAA_pred

# Compare the prediction "cones" visually
plot(test_ds_AAA_pred, xlab="Year", ylab="Temperature")


# Lets look at what our models actually are -- ETS
test_ds_AAA


test_ds_AAA2 <- forecast::holt(test_ds_kingston, h=960)
test_ds_AAA2
plot(test_ds_AAA2)

f_AAA  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA <- tsCV(test_ds_kingston, f_AAA, h=1, window=960)

par(mfrow=c(1,1)) 
plot(errors_AAA, ylab='tsCV errors')
abline(0,0)
legend("left", legend=c("CV_error_AAA"), col=c("black" ), lty=4:1)

mean(abs(errors_AAA/test_ds_kingston), na.rm=TRUE)*100

# Print the mean and confidence intervals for the AAA model
test_ds_AAA_pred

# Export the results out
write.csv(test_ds_AAA_pred, file = "Temperatures3.csv") # export the selected model's predictions into a CSV file


################################################################################
##########################                       ###############################
##########################       QUESTION 4      ###############################
##########################                       ###############################
################################################################################



nasa <- read_excel("C:\\Users\\Windows\\Downloads\\MMA867_Assignment2_dataset_Jaison.xlsx", 2)


nasa_clean <- gather(nasa, "month", "temp", -1)



nasa_order <- sqldf("select * from nasa_clean order by Year, month")

nasa_order$month_no <- ifelse(nasa_order$month == 'Jan', 1, 
                              ifelse(nasa_order$month == 'Feb', 2,
                                     ifelse(nasa_order$month == 'Mar', 3,
                                            ifelse(nasa_order$month == 'Apr', 4,
                                                   ifelse(nasa_order$month == 'May', 5,
                                                          ifelse(nasa_order$month == 'Jun', 6,
                                                                 ifelse(nasa_order$month == 'Jul', 7,
                                                                        ifelse(nasa_order$month == 'Aug', 8,
                                                                               ifelse(nasa_order$month == 'Sep', 9,
                                                                                      ifelse(nasa_order$month == 'Oct', 10,
                                                                                             ifelse(nasa_order$month == 'Nov', 11, 12)
                                                                                      ))))))))))

nasa_order$tempK <- celsius.to.kelvin(nasa_order$temp)

nasa_order <- sqldf("select * from nasa_order WHERE Year < 2015 order by Year, month_no")


plot.ts(nasa_order$tempK, xlab="Year", ylab="Anomaly Temperature (in K)")

train_ds <- ts(nasa_order$tempK, start=1880, frequency = 12)
plot(train_ds, xlim=c(2010,2015))


library(ggplot2)

mean2 <- window(train_ds,start=1880,end=c(2012,12))
mean1 <- meanf(mean2,h=12)
mean5 <- rwf(mean2,h=12)
mean3 <- snaive(mean2,h=12)
mean4 <- ses(mean2,h=12)
autoplot(window(train_ds, start=2010, end=2014)) +
  autolayer(mean1, series="Mean", PI=FALSE) +
  autolayer(mean5, series="Naïve", PI=FALSE) +
  autolayer(mean3, series="Seasonal naïve", PI=FALSE) +
  autolayer(mean4, series="SES", PI=FALSE) +  
  xlab("Time") + ylab("Temperature(K)") +
  ggtitle("Did Global Warming stop in 2013?") +
  guides(colour=guide_legend(title="Forecast"))

sequence_of_means <- sqldf("select distinct(Year), avg(tempK) as mean_tempK, stdev(tempK)  from nasa_order group by Year")
sequence_of_means
#2013 mean is 273.8350 and std_dev is 0.07681146

sequence_of_all_means <- sqldf("select avg(mean_tempK) as mean_of_means_tempK, stdev(mean_tempK) as stdev_tempK from sequence_of_means where Year < 2013")
sequence_of_all_means
#mean of all means is 273.1503 and stdev_tempK is 0.2987345

273.1503 - (1.96*0.2987345) # 272.5648 - Lower 97.5
273.1503 + (1.96*0.2987345) # 273.7358 - Higher 97.5 

################################################################################
##########################                       ###############################
##########################       QUESTION 5      ###############################
##########################                       ###############################
################################################################################


nasa_order <- sqldf("select * from nasa_clean order by Year, month")

nasa_order$month_no <- ifelse(nasa_order$month == 'Jan', 1, 
                              ifelse(nasa_order$month == 'Feb', 2,
                                     ifelse(nasa_order$month == 'Mar', 3,
                                            ifelse(nasa_order$month == 'Apr', 4,
                                                   ifelse(nasa_order$month == 'May', 5,
                                                          ifelse(nasa_order$month == 'Jun', 6,
                                                                 ifelse(nasa_order$month == 'Jul', 7,
                                                                        ifelse(nasa_order$month == 'Aug', 8,
                                                                               ifelse(nasa_order$month == 'Sep', 9,
                                                                                      ifelse(nasa_order$month == 'Oct', 10,
                                                                                             ifelse(nasa_order$month == 'Nov', 11, 12)
                                                                                      ))))))))))

nasa_order$tempK <- celsius.to.kelvin(nasa_order$temp)

nasa_order <- sqldf("select * from nasa_order order by Year, month_no")


sequence_of_stddev <- sqldf("select distinct(Year), stdev(tempK) as std_dev_tempK  from nasa_order group by Year")
sequence_of_stddev
#2013 stdev is 0.07681146 and 2020 std_dev is 0.04163332

sequence_of_all_stdev <- sqldf("select avg(std_dev_tempK) as avg_stdev, stdev(std_dev_tempK) as stdev from sequence_of_stddev")
sequence_of_all_stdev
#avg_stddev is 0.1068259 and stdev of all is 0.02969883

0.1068259 - (1.96*0.02969883)
0.1068259 + (1.96*0.02969883)


################################################################################
##########################                       ###############################
##########################       QUESTION 6      ###############################
##########################                       ###############################
################################################################################



nasa_order <- sqldf("select * from nasa_clean order by Year, month")

nasa_order$month_no <- ifelse(nasa_order$month == 'Jan', 1, 
                              ifelse(nasa_order$month == 'Feb', 2,
                                     ifelse(nasa_order$month == 'Mar', 3,
                                            ifelse(nasa_order$month == 'Apr', 4,
                                                   ifelse(nasa_order$month == 'May', 5,
                                                          ifelse(nasa_order$month == 'Jun', 6,
                                                                 ifelse(nasa_order$month == 'Jul', 7,
                                                                        ifelse(nasa_order$month == 'Aug', 8,
                                                                               ifelse(nasa_order$month == 'Sep', 9,
                                                                                      ifelse(nasa_order$month == 'Oct', 10,
                                                                                             ifelse(nasa_order$month == 'Nov', 11, 12)
                                                                                      ))))))))))

nasa_order <- sqldf("select * from nasa_order order by Year, month_no")

nasa_order$ID <- seq.int(nrow(nasa_order))

nasa_order$YearMonth <- paste(nasa_order$Year, "-" ,nasa_order$month_no)

nasa_order <- sqldf("select * from nasa_order where ID < 1684 order by Year, month_no")

nasa_order$tempK <- celsius.to.kelvin(nasa_order$temp)


train_2007<-subset(nasa_order, Year<=2006) #separate get pre2007
test_2007<-subset(nasa_order, Year>= 2007 & Year <=2017) #get 2007-2017

#TS
train_2007_ts <- ts(train_2007$tempK, start=1880, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 
test_2007_ts <- ts(test_2007$tempK,start=2007, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 

#ETS
ets_aaa_2007 <- ets(train_2007_ts, model="AAA",damped=FALSE)

#snaive
naive_2007<-naive(train_2007_ts,h=120,level=c(0.8, 0.95))
naive_2007

#create cone
cone_ets_aaa_2007 <- forecast(ets_aaa_2007, h=120, level=c(0.8, 0.95))


#plot
par(mfrow=c(1,2)) # This command sets the plot window to show 1 row of 4 plots
plot(cone_ets_aaa_2007, xlab="Year", ylab="Predicted Temp")
plot(naive_2007, xlab="Year", ylab="Predicted Temp")

### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout)

f_AAA_2017  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA_2017 <- tsCV(test_2007_ts, f_AAA_2017, h=1, window=60)

f_navie_2007  <- function(y, h) forecast(naive(y), h = h)
errors_naive_2017 <- tsCV(test_2007_ts, f_navie_2007, h=1, window=60)

#MAPE
mean(abs(errors_AAA_2017/test_2007_ts), na.rm=TRUE)*100
mean(abs(errors_naive_2017/test_2007_ts), na.rm=TRUE)*100


################################################################################
##########################                       ###############################
##########################       QUESTION 6      ###############################
##########################                       ###############################
################################################################################


train_1969<-subset(nasa_order, Year<=1969) #get pre1969
test_1969<-subset(nasa_order, Year>= 1970 & Year <=1980) #get 1970 - 1980

#TS
train_1969_ts <- ts(train_1969$tempK,start=1850, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 
test_1969_ts <- ts(test_1969$tempK,start=1969, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 

#notconstant
ets_aaa_1969 <- ets(train_1969_ts, model="AAA",damped=FALSE)

#constant
naive_1969<-naive(train_1969_ts,h=120,level=c(0.8, 0.95))
naive_1969

#create cone
cone_ets_aaa_1969 <- forecast(ets_aaa_1969, h=120, level=c(0.8, 0.95))

#plot
par(mfrow=c(1,2)) # This command sets the plot window to show 1 row of 4 plots
plot(cone_ets_aaa_1969, xlab="Year", ylab="Predicted Temp")
plot(naive_1969, xlab="Year", ylab="Predicted Temp")

### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout)

f_AAA_1969  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA_1069 <- tsCV(test_1969_ts, f_AAA_1969, h=1, window=60)

f_naive_1969  <- function(y, h) forecast(naive(y), h = h)
errors_naive_1069 <- tsCV(test_1969_ts, f_naive_1969, h=1, window=60)

#MAPE
mean(abs(errors_AAA_1069/test_1969_ts), na.rm=TRUE)*100
mean(abs(errors_naive_1069/test_1969_ts), na.rm=TRUE)*100

