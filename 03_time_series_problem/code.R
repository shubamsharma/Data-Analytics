PATH <-  "D:/09_analytics_new_start/06_time_series_problem/"
setwd(PATH)
data_path <- paste(PATH,"data/Train_SU63ISt.csv",sep = "")
data <- read.csv(data_path,stringsAsFactors = FALSE)
head(data$Datetime)


library(lubridate)
data$Datetime <- dmy_hm(data$Datetime)
head(data)
class(data$Datetime)

library(xts)
data.xts <- xts(x = data$Count, order.by = data$Datetime)
head(data)


data.xts.monthly = apply.monthly(data.xts,sum)
head(data.xts.monthly)

#Split the data
# In Case of time series prolem traditional spit is not used we directly dicive it sequentially(date)
data.xts.end <- floor( 0.8 * length(data.xts.monthly))
data.xts.train <- data.xts.monthly[1:data.xts.end]
data.xts.test <- data.xts.monthly[ (data.xts.end + 1) : length(data.xts.monthly)]


#Many function work best wiht base R 
#so convert xts to ts objects 
data.xts.start <- c(year(start(data.xts.train)), month(start(data.xts.train)), day(start(data.xts.train)))
data.xts.end <- c(year(end(data.xts.train)), month(end(data.xts.train)), day(end(data.xts.train)))
data.ts.train <- ts(as.numeric(data.xts.train),start = data.xts.start, end = data.xts.end, frequency = 12)

data.xts.start <- c(year(start(data.xts.test)), month(start(data.xts.test)), day(start(data.xts.test)))
data.xts.end <- c(year(end(data.xts.test)), month(end(data.xts.test)), day(end(data.xts.test)))
data.ts.test <- ts(as.numeric(data.xts.test),start = data.xts.start, end = data.xts.end, frequency = 12)

#par(mar=c(1,1,1,1))
plot(data.ts.train)
abline(reg = lm(data.ts.train~time(data.ts.train)))
boxplot(data.ts.train~cycle(data.ts.train))

start(data.ts.train)

end(data.ts.train)

library(tseries)

#AR I MA (Auto Regression) (Moving Average) Integration
#p  d  q 

acf(data.ts.train)

acf(diff(log(data.ts.train))) # Value of q = 2 - coefficient of ma

pacf(diff(log(data.ts.train))) # Value of p

plot(diff(log(data.ts.train))) 

fit <- arima(log(data.ts.train),c(0,1,1),seasonal = list(order = c(0,1,1), period = 12))

pred <- predict(fit,n.ahead = 1*12)

predt <- 2.718^pred$pred

ts.plot(data.ts.train,2.718*pred$pred,log = "y",ity = c(1.3))

#testing our Model

start(data.ts.train)
end(data.ts.train)

datawide <-ts(data.ts.train, frequency = 12, start = c(2012,8), end = c(2014,3))
 
fit <- arima(log(datawide),c(0,1,1),seasonal = list(order = c(0,1,1), period = 12))

pred <- predict(fit,n.ahead = 1*12)

predt <-2.718^pred$pred


data1 <- head(predt,12)

predict_2014 <-round(data1,digits = 0)

original_2014 <- tail(data.ts.train,12)

predict_2014

original_2014
