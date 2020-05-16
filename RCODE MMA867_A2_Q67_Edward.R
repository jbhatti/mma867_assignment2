
##*********************Q6**************************


#import data
post <- read_excel("C:\\Users\\School\\Desktop\\867\\post2007.xlsx")

#import data
pre <- read_excel("C:\\Users\\School\\Desktop\\867\\pre2007.xlsx")

#TS
pre2007_ts <- ts(pre$Temperature,start=1850, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 

#notconstant
met_aaa <- ets(metdata_ts, model="AAA",damped=FALSE)

#constant
constant_ann <- ets(metdata_ts, model="ANN",damped=FALSE)


#naive
fc<-snaive(pre$Temperature,h=120)

#forecast
forecast_aaa <- forecast(met_aaa, h=120, level=c(0.8, 0.95))
forecast_constant <- forecast(constant_ann, h=120, level=c(0.8, 0.95))

par(mfrow=c(1,2)) 
plot(forecast_aaa)
plot(forecast_constant)

#**************Q7*******************
#import data
pre1986 <- read_excel("C:\\Users\\School\\Desktop\\867\\pre1986.xlsx")  

#TS
pre1986_ts <- ts(pre1986$Temperature,start=1850, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 

#notconstant
met_aaa_1986 <- ets(pre1986_ts, model="AAA",damped=FALSE)

#constant
constant_1986 <- ets(pre1986_ts, model="ANN",damped=FALSE)


#forecast
forecast_aaa_1986 <- forecast(met_aaa_1986, h=120, level=c(0.8, 0.95))
forecast_constant_1986 <- forecast(constant_1986, h=120, level=c(0.8, 0.95))

#graph
par(mfrow=c(1,2)) 
plot(forecast_aaa_1986)
plot(forecast_constant_1986)
