# Install lubridate if not already installed
install.packages("lubridate")
library(fBasics) 
library(tseries)
library(astsa)
library(lmtest)
library(TSA)
library(dbplyr)
library(forecast)
library(lubridate)

#Importing the data
data = read.csv('Unemployment.csv')
head(data)

# Created Date Column
data$Year <- as.character(data$Year)
data$Month <- sprintf("%02d", as.numeric(data$Month)) 
data$Date <- as.Date(with(data, paste(Year, Month, '01' ,sep="-")), "%Y-%m-%d")
head(data)

# Check for missing values
sum(is.na(data))

# Remove commas and convert to numeric
data$Total_Unemployment <- as.numeric(gsub(",", "", data$Total_Unemployment))
str(data)

# EDA

# Filter rows for California and Los Angeles County
filtered_df <- data[data$State %in% c("California", "Los Angeles County"), ]

# Select relevant columns
filtered_df <- filtered_df[, c("State", "Date", "Percent_Unemployed")]

# View the first few rows
head(filtered_df)

# Let's plot both time series plots

# Plot for California and Los Angeles County using the plot() function
# Create an empty plot first for the initial line (California)
plot(filtered_df$Date[filtered_df$State == "California"], 
     filtered_df$Percent_Unemployed[filtered_df$State == "California"], 
     type = "l", # "l" is for lines
     col = "blue", # Line color for California
     xlab = "Date", 
     ylab = "Percent Unemployed",
     main = "Unemployment Trends for California and Los Angeles County",
     lwd = 2, # Line width
     xaxt = "n") # Disable x-axis (to customize it later)

# Add Los Angeles County line to the same plot
lines(filtered_df$Date[filtered_df$State == "Los Angeles County"], 
      filtered_df$Percent_Unemployed[filtered_df$State == "Los Angeles County"], 
      col = "red", # Line color for Los Angeles County
      lwd = 2) # Line width

# Customize x-axis labels for better readability
axis(1, at = seq(min(data$Date), max(data$Date), by = "1 year"),       # Adjust "by" to "1 year"
     labels = format(seq(min(data$Date), max(data$Date), by = "1 year"), "%Y"),  # Format to show only the year
     las = 2) 

# Add a legend to the plot
legend("topleft", legend = c("California", "Los Angeles County"), 
       col = c("blue", "red"), lwd = 2)


# Using aggregate to calculate the average unemployment rate per state
average_unemployment <- aggregate(data$Percent_Unemployed ~ data$State, data = data, FUN = mean)

# Rename the columns for easier understanding
colnames(average_unemployment) <- c("State", "Average_Unemployment_Rate")

# Sort the states by the average unemployment rate (descending order)
average_unemployment_sorted <- average_unemployment[order(average_unemployment$Average_Unemployment_Rate, decreasing = TRUE), ]

# Find the ranking of California
california_rank <- which(average_unemployment_sorted$State == "California")

# Get the unemployment rate for California
california_rate <- average_unemployment_sorted$Average_Unemployment_Rate[california_rank]

# Print the results
cat("California's ranking is:", california_rank, "\n")
cat("California's average unemployment rate is:", california_rate, "\n")

# Get the top 5 states with the highest unemployment rates
top_5_highest <- head(average_unemployment_sorted, 7)

# Get the top 5 states with the lowest unemployment rates
top_5_lowest <- tail(average_unemployment_sorted, 5)

# Print the results
print("Top 5 States with Highest Unemployment Rates:")
print(top_5_highest)

print("Top 5 States with Lowest Unemployment Rates:")
print(top_5_lowest)


# Comparing unemployment for states with highest and lowest unemployment rates 
# West Virginia and Nebraska

wv_data <- subset(data, State == "West Virginia")
n_data <- subset(data, State == "Nebraska")

# Plot Total_Unemployment for West Virgina
plot(wv_data$Date, wv_data$Percent_Unemployed, 
     type = "l", col = "blue", 
     xlab = "Date", ylab = "Unemployment Rate (%)", 
     main = "Unemployment Rate in West Virginia and Nebraska",
     lwd = 2, ylim = c(0, 20))  # Line width

# Add Total_Unemployment for Nebraska to the same plot
lines(n_data$Date, n_data$Percent_Unemployed, 
      col = "red", lwd = 2)

# Add a legend to differentiate the two states
legend("topright", legend = c("West Virginia", "Nebraska"), 
       col = c("blue", "red"), lty = 1, lwd = 2)


# Filter data for 1976
data_1976 <- subset(data, format(Date, "%Y") == "1976")
mean_1976 <- mean(data_1976$Percent_Unemployed, na.rm = TRUE)

# Filter data for 2023
data_2022 <- subset(data, format(Date, "%Y") == "2022")
mean_2022 <- mean(data_2022$Percent_Unemployed, na.rm = TRUE)

# Print results
cat("Mean Unemployment Rate in 1976:", mean_1976, "\n")
cat("Mean Unemployment Rate in 2022:", mean_2022, "\n")


# Find the row with the minimum unemployment rate
min_row <- california_data[which.min(california_data$Percent_Unemployed), ]

# Find the row with the maximum unemployment rate
max_row <- california_data[which.max(california_data$Percent_Unemployed), ]

# Display results
cat("Month with Minimum Unemployment Rate:\n")
print(min_row$Date)
cat("Rate: ", min_row$Percent_Unemployed, "\n\n")

cat("Month with Maximum Unemployment Rate:\n")
print(max_row$Date)
cat("Rate: ", max_row$Percent_Unemployed, "\n")



##########################################################
# Subset data for California
california_data <- subset(data, State == "California")

# Plot the unemployment rate over time 
plot(california_data$Date, california_data$Percent_Unemployed,
     type = "l",  # Line plot
     main = "Unemployment Rate in California State Over Time",
     xlab = "Date",
     ylab = "Unemployment Rate (%)",
     lwd = 1)

cutoff_date <- as.Date("2019-07-28")

# Test set: data from the cutoff date onward (Will look at the data prior to COVID-19)
test_data <- subset(california_data, Date > cutoff_date & Date <= as.Date("2020-02-29"))

train_data <- subset(california_data, Date <= cutoff_date)
test_dates=test_data$Date
# Check the dimensions of the training and test data
dim(train_data)
dim(test_data)

date= train_data$Date
rate= train_data$Percent_Unemployed

plot(rate, type = 'l', main = 'Unemployment Rate', xlab= 'Time', ylab='Rate')
# Data is not stationary. Taking log(x)
log_x=log(rate)
plot(date, log_x, type = 'l', main = 'Unemployment Rate log(x)', xlab= 'Time', ylab='Rate')
adf.test(log_x)
par(mfrow = c(1, 2))
# ACF anf PACF plot
acf(log_x,  main = "ACF of logx")
pacf(log_x, main = "PACF of logx")
# Although adf test shows stationarity, ACF and PACF plots indicate that data is still not stationary

# Taking a diff(log(x))
dlog_x=diff(log_x)
adf.test(dlog_x)

par(mfrow = c(1, 2))
acf(dlog_x,  main = "ACF of dlogx")
pacf(dlog_x, main = "PACF of dlogx")

# Based on adf test p-value=0.01  we confirm stationarity.
# Based on documentation provided, we know that seasonal component was removed.

eacf(dlog_x)

# Will look at ARMA(1,5), ARMA(1,9),  and ARMA(3,5) (ARMA(1,5) doesn't work based on eacf plot)

# ARMA(1,5)
out15= arima(dlog_x, order = c(1, 0, 5))
out15
coeftest(arma15)
out15.1= arima(dlog_x, order = c(1, 0, 5), fixed=c(NA,NA,NA,0,NA,NA,0))
out15.1
coeftest(out15.1)
residuals15=out15.1$residuals
plot(residuals15, type = 'l', main = 'Plot of Residuals', xlab= 'Time', ylab='Rate')
par(mfrow = c(1, 2))
acf(residuals15,  main = "ACF of residuals")
pacf(residuals15, main = "PACF of residuals")
Box.test(residuals15, lag = 12, type = 'Ljung')
abs(polyroot(c(1, -out15.1$coef[1]))) # Model is stationary
# Residuals Behave like white noise. Final AIC=-3416.3


# ARMA(1,9)
out19= arima(dlog_x, order = c(1, 0, 9))
out19
coeftest(out19)
out19.1= arima(dlog_x, order = c(1, 0, 9), fixed=c(NA,NA,NA,0,0,NA,0,0,0,NA,0))
out19.1
coeftest(out19.1)
residuals19=out19.1$residuals
plot(residuals19, type = 'l', main = 'Plot of Residuals', xlab= 'Time', ylab='Rate')
par(mfrow = c(1, 2))
acf(residuals19,  main = "ACF of residuals")
pacf(residuals19, main = "PACF of residuals")
Box.test(residuals19, lag = 12, type = 'Ljung')
abs(polyroot(c(1, -out19.1$coef[1]))) # Model is stationary
# Residuals Behave like white noise. Final AIC=-3416.27


out35= arima(dlog_x, order = c(3, 0, 5))
out35
# AIC = -3408.9 Lower than the other 2 options.
coeftest(out35)
out35.1= arima(dlog_x, order = c(3, 0, 5), fixed=c(NA,0,0,NA,0,0,0,NA,0))
out35.1 #Performance of the model became even worse after removing insignificant terms. It is not a good choice of model.


out45= arima(dlog_x, order = c(4, 0, 5))
out45
coeftest(out45)
out45.1= arima(dlog_x, order = c(4, 0, 5), fixed=c(NA,NA,NA,NA,NA,0,NA,NA,0,0))
out45.1
coeftest(out45.1)
residuals45=out45.1$residuals
plot(residuals45, type = 'l', main = 'Plot of Residuals', xlab= 'Time', ylab='Rate')
par(mfrow = c(1, 2))
acf(residuals45,  main = "ACF of residuals")
pacf(residuals45, main = "PACF of residuals")
Box.test(residuals45, lag = 12, type = 'Ljung')
abs(polyroot(c(1, -out45.1$coef[1:4]))) # Model is stationary
# Residuals Behave like white noise. Final AIC=-3416.27
# Final AIC= -3415.21

BIC(out15.1)
BIC(out19.1)
BIC(out45.1)

#Overall based on AIC and BIC ARMA(1,5) shows the best results.

source("rolling.forecast.R")

error1 = rolling.forecast(dlog_x, 5, length(dlog_x)-50, c(1,0,5), fixed=c(NA,NA,NA,0,NA,NA,0))

error2 = rolling.forecast(dlog_x, 5, length(dlog_x)-50, c(1,0,9),fixed=c(NA,NA,NA,0,0,NA,0,0,0,NA,0))

error3 = rolling.forecast(dlog_x, 5, length(dlog_x)-50, c(4,0,5), fixed=c(NA,NA,NA,NA,NA,0,NA,NA,0,0))


error1
error2
error3

error= c(error1, error2, error3)
par(mfrow=c(1,1))

plot(error1, type = 'l', ylim = c(min(error), max(error)), main = 'Rolling Forecasting Errors for Different Models', xlab = 'Forecast horizon', ylab = 'Error')
lines(error2, col = 2)
lines(error3, col = 3)

legend.text = c("ARIMA(1,0,5)","ARIMA(1,0,9)", "ARIMA(4,0,5)")
legend("bottomright", legend.text, lty = rep(1, 6), col = 1:6)


# Generate predictions for the next 7 steps
forecast <- predict(out15.1, n.ahead = 7)

# Get the last value of log_x before differencing
last_log_x <- tail(log_x, 1)

# Reverse the differencing: cumulative sum
pred <- cumsum(c(last_log_x, forecast$pred))
# Reversing log.
upper <- exp(pred[-1] + 1.96 * se)
lower <- exp(pred[-1] - 1.96 * se)
pred <- exp(pred)

# Define the last date of data
end_date <- as.Date("2019-07-28")

# Calculate the start date for the last 4 years
start_date <- end_date - years(4)

# Filter the data for the last 5 years
subset_data <- train_data[train_data$Date >= start_date & train_data$Date <= end_date, ]


# Determine the start and end dates
start_date <- min(subset_data$Date)  # The start date (training)
end_date <- max(subset_data$Date)    # The end date (training)

# Add 3 years to the end date
end_date_plus_2_years <- as.Date(end_date) + 365*3

# Ensure test predictions and intervals are available
# Ensure the lengths of the test data and predictions are the same
length(test_dates)  # Should match pred, upper, lower

# Plot historical data (training data)

plot(subset_data$Date, subset_data$Percent_Unemployed, 
     type = "l", 
     col = "black", 
     lwd = 2, 
     main = "Unemployment Rate Predictions for California State", 
     xlab = "Date", 
     ylab = "Unemployment Rate", 
     xlim = c(start_date, end_date_plus_2_years),
     ylim = range(c(subset_data$Percent_Unemployed, test_data$Percent_Unemployed, lower, upper)))  # Set y-axis range to include both test data and prediction intervals

# Add test data (actual test data)
lines(test_dates, test_data$Percent_Unemployed, col = "green", type = "o", pch = 16)  # Test data (green)

# Add predictions (forecasted test data)
lines(test_dates, pred[-1], col = "blue", type = "o", pch = 16)  # Predictions (blue)

# Add prediction intervals (95% CI)
lines(test_dates, upper, col = "red", lty = 2)  # Upper bound
lines(test_dates, lower, col = "red", lty = 2)  # Lower bound

# Add a legend
legend("topright", 
       legend = c("Historical Data", "Test Data", "Predictions", "95% CI"), 
       col = c("black", "green", "blue", "red"), 
       lty = c(1, 1, 1, 2), 
       pch = c(NA, 16, 16, NA), 
       lwd = c(2, 2, 2, 1.5))


# My Forecasting Results are not very accurate
# I will attempt different approached.
# Going back to diff(log(x))
adf.test(dlog_x)

par(mfrow = c(1, 2))
acf(dlog_x,  main = "ACF of dlogx")
pacf(dlog_x, main = "PACF of dlogx")

adf.test(log_x)
#Since adf of log_x suggested stationarity I tried to proceed with it.
eacf(log_x)
out25= arima(log_x, order = c(2, 0, 5))
out25
# AIC is better. -3421.28
coeftest(out25)
out25.1= arima(log_x, order = c(2, 0, 5), fixed=c(NA,NA,NA,NA,0,0,NA,NA))
out25.1 # AIC Further improved aic = -3423.18
coeftest(out25.1)
residuals25=out25.1$residuals
plot(residuals25, type = 'l', main = 'Plot of Residuals', xlab= 'Time', ylab='Rate')
par(mfrow = c(1, 2))
acf(residuals25,  main = "ACF of residuals")
pacf(residuals25, main = "PACF of residuals")
Box.test(residuals25, lag = 12, type = 'Ljung')
abs(polyroot(c(1, -out25.1$coef[c(1,2)])))
BIC(out25.1) #BIC=-3391.364

log_x.out15= arima(log_x, order = c(1, 0, 5))
log_x.out15 #Better aic = -3348.77
coeftest(log_x.out15)
residuals15=log_x.out15$residuals
plot(residuals15, type = 'l', main = 'Plot of Residuals', xlab= 'Time', ylab='Rate')
par(mfrow = c(1, 2))
acf(residuals15,  main = "ACF of residuals")
pacf(residuals15, main = "PACF of residuals")
Box.test(residuals15, lag = 12, type = 'Ljung')
# Residuals don't behave like white noise and therefore this model isn't good.

# Will continue with model ARMA(2,0,5)
forecast <- predict(out25.1, n.ahead = 7)
pred=forecast$pred
pred
se = forecast$se
upper <- exp(pred + 1.96 * se)
lower <- exp(pred - 1.96 * se)
pred = exp(pred)
pred

plot(subset_data$Date, subset_data$Percent_Unemployed, 
     type = "l", 
     col = "black", 
     lwd = 2, 
     main = "Unemployment Rate Predictions for California State", 
     xlab = "Date", 
     ylab = "Unemployment Rate", 
     xlim = c(start_date, end_date_plus_2_years),
     ylim = range(c(subset_data$Percent_Unemployed, test_data$Percent_Unemployed, lower, upper)))  # Set y-axis range to include both test data and prediction intervals

# Add test data (actual test data)
lines(test_dates, test_data$Percent_Unemployed, col = "green", type = "o", pch = 16)  # Test data (green)

# Add predictions (forecasted test data)
lines(test_dates, pred, col = "blue", type = "o", pch = 16)  # Predictions (blue)

# Add prediction intervals (95% CI)
lines(test_dates, upper, col = "red", lty = 2)  # Upper bound
lines(test_dates, lower, col = "red", lty = 2)  # Lower bound

# Add a legend
legend("topright", 
       legend = c("Historical Data", "Test Data", "Predictions", "95% CI"), 
       col = c("black", "green", "blue", "red"), 
       lty = c(1, 1, 1, 2), 
       pch = c(NA, 16, 16, NA), 
       lwd = c(2, 2, 2, 1.5))
# Despite the fact that ACF and PACF of Log(x) didn't appear stationary proceeding with
# log(x) model resulted in the lowest AIC and BIC as well as the best prediction.
