library(fBasics) # Load the package fBasics.
library(tseries)
library(astsa)
library(forecast)
library(fUnitRoots)
library(TSA)
library(lmtest)
library(ggplot2)
library(reshape2)
install.packages("reshape2")

data = read.csv("/Users/sabnampandit/Desktop/MIS748/Rcodes/unemployment_rate_data (1).csv", header = T) 
head(data)


# Convert the Date column to Date format
data$Date = as.Date(data$date, format = "%m/%d/%Y")
Date=data$Date 

# Extract the year and create a new column
data$Year = format(data$Date, "%Y")
Year=data$Year

# View the updated dataset
head(data)

# Visualize and handle missing values
summary(data)
data <- na.omit(data)  # Remove rows with missing values


# Calculate min, max, and mean for each category
stats <- data.frame(
  variable = c("unrate", "unrate_men", "unrate_women"),
  min = sapply(c("unrate", "unrate_men", "unrate_women"), function(x) min(data[[x]], na.rm = TRUE)),
  max = sapply(c("unrate", "unrate_men", "unrate_women"), function(x) max(data[[x]], na.rm = TRUE)),
  mean = sapply(c("unrate", "unrate_men", "unrate_women"), function(x) mean(data[[x]], na.rm = TRUE))
)

# Reshape data for the boxplot
df_melted <- melt(data, id.vars = c("Year"), measure.vars = c("unrate", "unrate_men", "unrate_women"))

# Plot boxplot with min, max, and mean values
ggplot(df_melted, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_text(data = stats, aes(x = variable, y = max, label = paste("Max:", round(max, 2))), vjust = -1) +
  geom_text(data = stats, aes(x = variable, y = min, label = paste("Min:", round(min, 2))), vjust = 1.5) +
  geom_text(data = stats, aes(x = variable, y = mean, label = paste("Mean:", round(mean, 2))), vjust = -1) +
  labs(title = "Distribution of Unemployment Rates",
       x = "Category",
       y = "Unemployment Rate (%)") +
  theme_minimal()



# Filter relevant columns for the time series
unemployment_rate <- ts(data$unrate_men, start = c(1948, 1), end = c(2015, 12), frequency = 12)
unemployment_rate

all_unemployment_rate = ts(data$unrate, start = c(1948, 1), frequency = 12)
length(all_unemployment_rate)


# Plot the time series data
plot(unemployment_rate, main = "US Unemployment Rate (1948-2015)", ylab = "Rate", xlab = "Year")
adf.test(unemployment_rate) # It is non stationary

par(mfrow = c(1,2))
acf(unemployment_rate, lag = 60)
pacf(unemployment_rate, lag = 60)
adf.test(unemployment_rate) # It is non stationary


#Since it has no constant mean and variance.
logx=log(unemployment_rate)
plot(logx,type='l', main="Logged Data")

dlogx <- diff(logx)
plot(dlogx, type = 'l', main = "Differenced Log Time Series")
adf.test(dlogx)
# Take seasonal difference
ddlogx <- diff(dlogx, lag = 12)
plot(ddlogx, type = 'l', main = "Seasonally Differenced Log Series")
adf.test(ddlogx)


par(mfrow = c(1,2))
acf(ddlogx,  main = "ACF of Seasonally Differenced Data", lag.max = 60)
pacf(ddlogx, main = "PACF of Seasonally  Differenced Data", lag.max = 60)





# Seasonal ARM(3,0,0)
out1 = arima(ddlogx, order = c(0, 0, 0), seasonal = list(order = c(3, 0, 0), period = 12))
out1
coeftest(out1)
out1$aic

# Seasonal ARM(0,0,1)
out1 = arima(ddlogx, order = c(0, 0, 0), seasonal = list(order = c(0, 0, 1), period = 12))
out1
coeftest(out1)
out1$aic


#Intercept is not significant, and we can remove it.

out11 = arima(logx, order = c(0, 1, 0), seasonal = list(order=c(0, 1, 1), period = 12))#When difference order is not zero, the intercept is dropped in the model.
out11
coeftest(out11)


par(mfrow = c(1,2))
acf(out11$residuals, lag.max = 60)
pacf(out11$residuals, lag.max = 60)

eacf(out11$residuals)


#ARIMA(6,0,6)xSARIMA(0,0,1)
out2 = arima(logx, order = c(6, 1, 6), seasonal = list(order=c(0,1,1),period = 12))
out2
coeftest(out2)
out2$aic

out21 = arima(logx, order = c(6,1,6), seasonal = list(order=c(0,1,1), period=12), fixed=c(0,NA,NA,NA,NA,NA,0,0,NA,NA,NA,NA,NA))
out21
coeftest(out21)
out21$aic

par(mfrow=c(1,2))
acf(out2$residuals)
pacf(out2$residuals)

Box.test(out2$residuals, type = 'Ljung',lag = 60)



# stationarity

#Model out2
print(out21$coef)
polyroot(c(1, -out2$coef[1:6])) 
abs(polyroot(c(1, -out2$coef[1:6]))) #roots are outside the unit cycle. greater than 1.It is stationary

polyroot(c(1, out2$coef[7:12])) 
abs(polyroot(c(1, out2$coef[7:12])))




################################ 
# Our best model is ARIMA(6,0,6)xSARIMA(0,0,1)



# PREDICTION CODE




all_unemployment_rate = ts(data$unrate_men, start = c(1948, 1), end = c(2020, 12),frequency = 12)

pp <- predict(out22.1, n.ahead = 60)
pred <- ts(exp(pp$pred), start = c(2016, 1), frequency = 12)
pred.low <- ts(exp(pp$pred - 2 * pp$se), start = c(2016, 1), frequency = 12)
pred.upp <- ts(exp(pp$pred + 2 * pp$se), start = c(2016, 1), frequency = 12)

# Plot actual and predicted values
plot(all_unemployment_rate, xlim = c(2008, 2021), ylim = c(min(all_unemployment_rate), max(all_unemployment_rate)), type = 'o', xlab = "Year", ylab = "Rate", main = "US Unemployment Rate Prediction for Men")
lines(pred, col = 'red')
lines(pred.low, col = 'red', lty = 2)
lines(pred.upp, col = 'red', lty = 2)
points(pred, col = 'red', pch = 2)
legend("bottomleft", legend = c("Actual Values", "Predicted Values", "Prediction Interval"), col = c("black", "red", "red"), lty = c(1, 1, 2), pch = c(1, 2, NA))

