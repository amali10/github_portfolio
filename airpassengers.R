library(tidyverse)
install.packages("zoo")
library(zoo)
data()
AirPassengers
is.na(AirPassengers)
class(AirPassengers)
start(AirPassengers);end(AirPassengers);frequency(AirPassengers)
ap_df <- data.frame(
  date=as.yearmon(time(AirPassengers)),
  value=as.numeric(AirPassengers)
)
ggplot(ap_df,aes(date,value))+
  geom_line(color="darkred")+
  labs(title = "airpassesngers monthly data",
       x="date",
       y="passengers(thousands)")+
  theme_minimal()

#yearwise
apdf1 <- data.frame(
  year=floor(time(AirPassengers)),
  value=as.numeric(AirPassengers)
)
yearly_summary <- aggregate(value~year,data = apdf1,
                            FUN = function(x)c(mean=mean(x),
                                                variance=var(x),
                                                min=min(x),
                                                max=max(x)))
yearly_summary <- do.call(data.frame,yearly_summary)
print(yearly_summary)

ap_df=data.frame(
  year=floor(time(AirPassengers)),
  value=as.numeric(AirPassengers)
)
yearly_mean=aggregate(value~year,data=ap_df,FUN = mean)
plot(yearly_mean$year,yearly_mean$value,
     type = "b",col="blue",
     main = "year-wise mean passengers",
     xlab = "year",ylab="mean passengers")

#monthwise
# Month-wise average passengers
ap_df <- data.frame(
  month = cycle(AirPassengers),          
  value = as.numeric(AirPassengers)     
)

monthly_mean <- aggregate(value ~ month, data = ap_df, FUN = mean)

plot(monthly_mean$month, monthly_mean$value,
     type = "b", col = "blue",
     main = "Month-wise Average Passengers (1949–1960)",
     xlab = "Month", ylab = "Average Passengers",
     xaxt = "n")                         
axis(1, at = 1:12, labels = month.abb)   

#summary
data <- as.numeric(AirPassengers)
months <- cycle(AirPassengers)
table(months)  
install.packages("e1071")
library(e1071)

skewness(data)
summary(AirPassengers)

hist(AirPassengers,
     main = "histogram of AirPassengers",
     xlab="passengers",
     col = "lightblue")

plot(density(AirPassengers),
     main = "density plot of airpassengers",
     xlab ="passengers",
     col="red")

boxplot(AirPassengers,
        main="boxplot of aipassengers",
        xlab="passengers",
        col="lightgreen")
barplot(monthly_mean$value,
        names.arg = month.abb,
        main="average passengers ny month",
        xlab="month",ylab = "avearge passengers",
        col="orange")
plot(data, main = "AirPassengers Time Series")
decomp <- decompose(AirPassengers)
plot(decomp)
 
library(zoo)

rollmean <- rollmean(AirPassengers, 12)   
plot(rollmean, col = "blue", main = "Rolling Mean (12 months)")

rollvar <- rollapply(AirPassengers, 12, var)                             
plot(rollvar, col = "red", main = "Rolling Variance (12 months)")
 
install.packages("tseries")
library(tseries)

adf.test(data)
acf(data, main = "ACF of AirPassengers")
pacf(data, main = "PACF of AirPassengers")

log_data <- log(AirPassengers)
plot(log_data, main = "Log-transformed AirPassengers")

diff_data <- diff(log_data)     
plot(diff_data, main = "Differenced Log AirPassengers")

diff_seasonal <- diff(log_data, lag = 12) 
plot(diff_seasonal, main = "Seasonally Differenced Log AirPassengers")

install.packages("forecast")
library(forecast)
seasonplot(AirPassengers, col = rainbow(12),
           main = "Seasonal Plot: AirPassengers",
           year.labels = TRUE, year.labels.left = TRUE)

library(ggplot2)
install.packages("reshape2")
library(reshape2)

ap_df <- data.frame(
  year  = floor(time(AirPassengers)),
  month = cycle(AirPassengers),
  value = as.numeric(AirPassengers)
)

ap_wide <- dcast(ap_df, year ~ month, value.var = "value")

# Heatmap
install.packages("pheatmap")
library(pheatmap)
pheatmap(ap_wide[,-1], cluster_rows = FALSE, cluster_cols = FALSE,
         main = "Heatmap of Monthly Passenger Counts",
         labels_row = ap_wide$year, labels_col = month.abb)

lag.plot(AirPassengers, lags = 12, do.lines = FALSE)
