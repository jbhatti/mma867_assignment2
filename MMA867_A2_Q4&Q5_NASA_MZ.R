library(tidyr)
library(readxl)
library(sqldf)
library(forecast)
library(weathermetrics)
library(ggplot2)

nasa <- read_excel("/Users/molly/Desktop/MMA/OneDrive - Queen's University/MMA867/Group Assignment 1/Molly Work Q4&5/MMA867_A2_Q4&Q5_NASA_DATASET_MZ.xlsx", sheet=2)

#CLEANING DATA

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
nasa_order$tempK <-celsius.to.kelvin(nasa_order$temp)
write.csv(nasa_order,file="nasa_order.csv")


#########################################  Q 4   ##############################################
nasa_order_1 <- sqldf("select * from nasa_order where ID < 1683 order by Year, month_no")

#chart of temperature in farenheit from 1880-2012#
plot.ts(nasa_order_1$tempK)
nasa_order_1.1 <- sqldf("select ID, Year, month_no, tempK from nasa_order_1 where ID < 1597 order by Year, month_no")

#decomposing using additive type 
test_ds <- ts(nasa_order_1.1$tempK, start=1880, frequency = 12)
fit <- decompose(test_ds, type='additive')
plot(fit)
 

#decomposing using STL
fit <- stl(test_ds, t.window=12, s.window='periodic', robust=TRUE)
plot(fit)
#conclusion: Data CI is low, Seasonal CI is high, Trend CI is low, Reminder CI is low


#create exponential smoothing models - AAA
test_ds_AAA <- ets(test_ds, model="AAA", damped=FALSE)
test_ds_AAA
#conclusion: alpha = 0.4661, beta = 1e-04, gamma = 0.067 

#create prediction "cones" for 36 months (3 years) into the future with quintile confidence intervals (2013-2015)
test_ds_AAA_pred <- forecast(test_ds_AAA, h=36, level=c(0.8, 0.95))
plot(test_ds_AAA_pred, xlim=range(2010,2015))
test_ds_AAA_pred
write.csv(test_ds_AAA_pred, file="Q4_pred.csv")

#comparison
Q4_pred<-read.csv("/Users/molly/Desktop/MMA/OneDrive - Queen's University/MMA867/Group Assignment 1/Molly Work Q4&5/Q4_pred.csv")
real_temp <-nasa_order_1$tempK[1597:1632]
Q4_pred$real<-real_temp
write.csv(Q4_pred, file="Q4_analysis.csv")


#########################################  Q 5   ##############################################
#chart of temperature in farenheit from 1880-2020 Feb#

nasa_order_2.1 <- sqldf("select ID, Year, month_no, tempK from nasa_order_1 where ID < 1683 order by Year, month_no")

#decomposing using additive type 
test_ds_2 <- ts(nasa_order_2.1$tempK, start=1880, frequency = 12)
fit <- decompose(test_ds_2, type='additive')
plot(fit)


#decomposing using STL
fit <- stl(test_ds_2, t.window=12, s.window='periodic', robust=TRUE)
plot(fit)
#conclusion: Data CI is low, Seasonal CI is high, Trend CI is low, Reminder CI is low


#create exponential smoothing models - AAA
test_ds_AAA_2 <- ets(test_ds_2, model="AAA", damped=FALSE)
test_ds_AAA_2
#conclusion: alpha = 0.4773, beta = 1e-04, gamma = 0.0657

#create prediction "cones" for 36 months (3 years) into the future with quintile confidence intervals (2020-2023)
test_ds_AAA_pred_2 <- forecast(test_ds_AAA_2, h=36, level=c(0.8, 0.95))
plot(test_ds_AAA_pred_2, xlim=range(2018,2023))
test_ds_AAA_pred_2
write.csv(test_ds_AAA_pred_2, file="Q5_pred.csv")




