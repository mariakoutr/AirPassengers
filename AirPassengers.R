## Load data & libraries 

data("AirPassengers")
air <- AirPassengers

library(tseries)
library(forecast)

## Explore the data 

class(air)
start(air)
end(air)

frequency(air)

summary(air)

png("plot1.png")
plot(air, ylab = "Number of passengers", main = "Air Passengers")
abline(reg = lm(air~time(air)))
dev.off()

cycle(air)

png("plot2.png")
plot(aggregate(air, FUN = mean), ylab = "Number of passenger", 
     main = "Air Passengers")
dev.off()

png("plot3.png")
plot(aggregate(air, FUN = var), ylab = "Variance of the Number of passenger",
     main = "Air Passengers Variance")
dev.off()

png("boxplot1.png")
boxplot(air~cycle(air), ylab = "Number of passenger", xlab = "Months", 
        main = "Boxplot of Air Passengers by month")
dev.off()

## Decompose the data 

air_decomp <- decompose(air)

png("plot4.png")
plot(air_decomp)
dev.off()

ndiffs(air)

adf.test(diff(log(air)), alternative = "stationary", k =0)

## ACF/PCAF plots and find optimal parameters

png("acf/pacf.png")
tsdisplay(diff(log(air)), main = "Stationarized Time Series")
dev.off()

png("acf/pacf_random.png")
tsdisplay(decompose(log(air))$random, main = 
                  "Random component of stationarized time series")
dev.off()

arimaMod <- auto.arima(log(air), d=1)
arimaMod 

## White Noise

png("acf/pacf_res.png")
tsdisplay(arimaMod$residuals) 
dev.off()

## Prediction 

pred <- predict(arimaMod, n.ahead = 120)

png("forecast_plot.png")
ts.plot(air,2.718^pred$pred, log = "y", lty = c(1,3), col = c("black", "red"), 
        ylab = "Number of Air Passengers", 
        main = "Actual and 10-year Forecast Number of the Air Passengers")
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"),
       lty = c(1,3))
dev.off()
