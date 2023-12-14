# Load required libraries
library(tidyverse)
library(caret)
library(neuralnet)
library(forecast)
library(dplyr)
library(Metrics)
library(tseries)

crude_oil_prices= read.csv("Proj.csv")

summary(crude_oil_prices)

# Line plot of crude oil prices
ggplot(crude_oil_prices, aes(x = Year, y = Cushing)) +
  geom_line() +
  ggtitle("Average Yearly Cushing Prices (1986-2016)") +
  xlab("Year") +
  ylab("Average Price (USD/barrel)")

ggplot(crude_oil_prices, aes(x = Year, y = Europe)) +
  geom_line() +
  ggtitle("Average Yearly Europe Brent Spot Price FOB (Dollars per Barrel) (1986-2016)") +
  xlab("Year") +
  ylab("Average Price (USD/barrel)")

ggplot(crude_oil_prices, aes(x = Year, y = NY)) +
  geom_line() +
  ggtitle("Average Yearly New York Harbor Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon) Prices (1986-2016)") +
  xlab("Year") +
  ylab("Average Price (USD/barrel)")

ggplot(crude_oil_prices, aes(x = Year, y = Gasoline_us)) +
  geom_line() +
  ggtitle("Average Yearly U.S. Gulf Coast Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon) Prices (1986-2016)") +
  xlab("Year") +
  ylab("Average Price (USD/barrel)")

ggplot(crude_oil_prices, aes(x = Year, y = gasoline_la)) +
  geom_line() +
  ggtitle("Average Yearly Los Angeles Reformulated RBOB Regular Gasoline Spot Price (Dollars per Gallon) Prices (1986-2016)") +
  xlab("Year") +
  ylab("Average Price (USD/barrel)")

# Extract decade information from Year
crude_oil_prices$Decade <- as.factor((crude_oil_prices$Year %/% 10) * 10)

ggplot(crude_oil_prices, aes(x = Decade, y = Cushing)) +
  geom_boxplot() +
  ggtitle("Average Yearly Cushing Prices by Decade") +
  xlab("Decade") +
  ylab("Average Price (USD/barrel)")

ggplot(crude_oil_prices, aes(x = Decade, y = Europe)) +
  geom_boxplot() +
  ggtitle("Average Yearly Europe Brent Spot Price FOB (Dollars per Barrel) Prices by Decade") +
  xlab("Decade") +
  ylab("Average Price (USD/barrel)")

ggplot(crude_oil_prices, aes(x = Decade, y = NY)) +
  geom_boxplot() +
  ggtitle("Average Yearly New York Harbor Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon) Prices by Decade") +
  xlab("Decade") +
  ylab("Average Price (USD/barrel)")

ggplot(crude_oil_prices, aes(x = Decade, y = Gasoline_us)) +
  geom_boxplot() +
  ggtitle("Average Yearly U.S. Gulf Coast Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon) Prices by Decade") +
  xlab("Decade") +
  ylab("Average Price (USD/barrel)")

ggplot(crude_oil_prices, aes(x = Decade, y = gasoline_la)) +
  geom_boxplot() +
  ggtitle("Average Yearly Los Angeles Reformulated RBOB Regular Gasoline Spot Price (Dollars per Gallon) Prices by Decade") +
  xlab("Decade") +
  ylab("Average Price (USD/barrel)")

# Histogram of crude oil prices
ggplot(crude_oil_prices, aes(x = Cushing)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Cushing Prices") +
  xlab("Average Cushing Price (USD/barrel)") +
  ylab("Frequency")

ggplot(crude_oil_prices, aes(x = Europe)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Europe Brent Spot Price FOB (Dollars per Barrel) Prices") +
  xlab("Average Europe Brent Spot Price FOB (Dollars per Barrel) (USD/barrel)") +
  ylab("Frequency")

ggplot(crude_oil_prices, aes(x = NY)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  ggtitle("Distribution of New York Harbor Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon) Prices") +
  xlab("Average New York Harbor Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon) Price (USD/barrel)") +
  ylab("Frequency")

ggplot(crude_oil_prices, aes(x = Gasoline_us)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  ggtitle("Distribution of U.S. Gulf Coast Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon) Prices") +
  xlab("Average U.S. Gulf Coast Conventional Gasoline Regular Spot Price FOB (Dollars per Gallon) Price (USD/barrel)") +
  ylab("Frequency")

ggplot(crude_oil_prices, aes(x = gasoline_la)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Los Angeles Reformulated RBOB Regular Gasoline Spot Price (Dollars per Gallon) Prices") +
  xlab("Average Los Angeles Reformulated RBOB Regular Gasoline Spot Price (Dollars per Gallon) Price (USD/barrel)") +
  ylab("Frequency")

# Density plot of crude oil prices
ggplot(crude_oil_prices, aes(x = Average_Price)) +
  geom_density(fill = "skyblue", color = "black") +
  ggtitle("Density Plot of Crude Oil Prices") +
  xlab("Average Crude Oil Price (USD/barrel)") +
  ylab("Density")

# Time series decomposition
crude_oil_cushing <- ts(crude_oil_prices$Cushing, start = c(1986,1), end = c(2016,1), frequency = 12)
decomp_cushing <- decompose(crude_oil_cushing)
plot(decomp_cushing)
crude_oil_europe <- ts(crude_oil_prices$Europe, start = c(1986,1), end = c(2016,1), frequency = 12)
decomp_europe <- decompose(crude_oil_europe)
plot(decomp_europe)
crude_oil_ny <- ts(crude_oil_prices$NY, start = c(1986,1), end = c(2016,1), frequency = 12)
decomp_ny <- decompose(crude_oil_ny)
plot(decomp_ny)
crude_oil_gasoline_us <- ts(crude_oil_prices$Gasoline_us, start = c(1986,1), end = c(2016,1), frequency = 12)
decomp_gasoline_us <- decompose(crude_oil_gasoline_us)
plot(decomp_gasoline_us)
crude_oil_gasoline_la <- ts(crude_oil_prices$gasoline_la, start = c(1986,1), end = c(2016,1), frequency = 12)
decomp_gasoline_la <- decompose(crude_oil_gasoline_la)
plot(decomp_gasoline_la)

#Exponential Smoothing method for forecasting
crude_oil_cushing <- ts(crude_oil_prices$Cushing, start = c(1986), end = c(2016))
fit <- ets(crude_oil_cushing, model = "AAN")
forecast <- forecast(fit, h = 5)
actual <- crude_oil_prices$Cushing[(nrow(crude_oil_prices)-4):nrow(crude_oil_prices)]
mae <- mean(abs(actual - forecast$mean))
rmse <- sqrt(mean((actual - forecast$mean)^2))
mape <- mean(abs((actual - forecast$mean)/actual)) * 100
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAPE = %.2f%%\n", mape))
plot(forecast, main = "Exponential Smoothing Forecast for Cushing Prices")
lines(forecast$mean, col = "blue")
lines(forecast$upper, col = "red", lty = "dashed")
lines(forecast$lower, col = "red", lty = "dashed")
legend("topleft", legend = c("Actual", "Forecast", "95% CI"), col = c("black", "blue", "red"), lty = c(1, 1, 2), bty = "n")

crude_oil_europe <- ts(crude_oil_prices$Europe, start = c(1986), end = c(2016))
fit <- ets(crude_oil_europe, model = "AAN")
forecast <- forecast(fit, h = 5)
actual <- crude_oil_prices$Europe[(nrow(crude_oil_prices)-4):nrow(crude_oil_prices)]
mae <- mean(abs(actual - forecast$mean))
rmse <- sqrt(mean((actual - forecast$mean)^2))
mape <- mean(abs((actual - forecast$mean)/actual)) * 100
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAPE = %.2f%%\n", mape))
plot(forecast, main = "Exponential Smoothing Forecast for Europe Prices")
lines(forecast$mean, col = "blue")
lines(forecast$upper, col = "red", lty = "dashed")
lines(forecast$lower, col = "red", lty = "dashed")
legend("topleft", legend = c("Actual", "Forecast", "95% CI"), col = c("black", "blue", "red"), lty = c(1, 1, 2), bty = "n")

crude_oil_ny <- ts(crude_oil_prices$NY, start = c(1986), end = c(2016))
fit <- ets(crude_oil_ny, model = "AAN")
forecast <- forecast(fit, h = 5)
actual <- crude_oil_prices$NY[(nrow(crude_oil_prices)-4):nrow(crude_oil_prices)]
mae <- mean(abs(actual - forecast$mean))
rmse <- sqrt(mean((actual - forecast$mean)^2))
mape <- mean(abs((actual - forecast$mean)/actual)) * 100
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAPE = %.2f%%\n", mape))
plot(forecast, main = "Exponential Smoothing Forecast for NY Prices")
lines(forecast$mean, col = "blue")
lines(forecast$upper, col = "red", lty = "dashed")
lines(forecast$lower, col = "red", lty = "dashed")
legend("topleft", legend = c("Actual", "Forecast", "95% CI"), col = c("black", "blue", "red"), lty = c(1, 1, 2), bty = "n")

crude_oil_gasoline_us <- ts(crude_oil_prices$Gasoline_us, start = c(1986), end = c(2016))
fit <- ets(crude_oil_gasoline_us, model = "AAN")
forecast <- forecast(fit, h = 5)
actual <- crude_oil_prices$Gasoline_us[(nrow(crude_oil_prices)-4):nrow(crude_oil_prices)]
mae <- mean(abs(actual - forecast$mean))
rmse <- sqrt(mean((actual - forecast$mean)^2))
mape <- mean(abs((actual - forecast$mean)/actual)) * 100
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAPE = %.2f%%\n", mape))
plot(forecast, main = "Exponential Smoothing Forecast for Gasoline_us Prices")
lines(forecast$mean, col = "blue")
lines(forecast$upper, col = "red", lty = "dashed")
lines(forecast$lower, col = "red", lty = "dashed")
legend("topleft", legend = c("Actual", "Forecast", "95% CI"), col = c("black", "blue", "red"), lty = c(1, 1, 2), bty = "n")

crude_oil_gasoline_la <- ts(crude_oil_prices$gasoline_la, start = c(1986), end = c(2016))
fit <- ets(crude_oil_gasoline_la, model = "AAN")
forecast <- forecast(fit, h = 5)
actual <- crude_oil_prices$gasoline_la[(nrow(crude_oil_prices)-4):nrow(crude_oil_prices)]
mae <- mean(abs(actual - forecast$mean))
rmse <- sqrt(mean((actual - forecast$mean)^2))
mape <- mean(abs((actual - forecast$mean)/actual)) * 100
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAPE = %.2f%%\n", mape))
plot(forecast, main = "Exponential Smoothing Forecast for Gasoline_la Prices")
lines(forecast$mean, col = "blue")
lines(forecast$upper, col = "red", lty = "dashed")
lines(forecast$lower, col = "red", lty = "dashed")
legend("topleft", legend = c("Actual", "Forecast", "95% CI"), col = c("black", "blue", "red"), lty = c(1, 1, 2), bty = "n")

#Regression
set.seed(123)
trainIndex <- createDataPartition(crude_oil_prices$Cushing, p = .7, list = FALSE)
train <- crude_oil_prices[trainIndex, ]
test <- crude_oil_prices[-trainIndex, ]
lm.fit <- lm(Cushing ~ Year, data = train)
pred <- predict(lm.fit, newdata = test)
mae <- mean(abs(pred - test$Cushing))
rmse <- sqrt(mean((pred - test$Cushing)^2))
mape <- mean(abs((pred - test$Cushing)/pred)) * 100
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAPE = %.2f%%\n", mape))
results <- data.frame(actual = test$Cushing, predicted = pred)
ggplot(results, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Cushing Price - Actual vs Predicted",
       x = "Actual",
       y = "Predicted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

set.seed(123)
trainIndex <- createDataPartition(crude_oil_prices$Europe, p = .7, list = FALSE)
train <- crude_oil_prices[trainIndex, ]
test <- crude_oil_prices[-trainIndex, ]
lm.fit <- lm(Europe ~ Year, data = train)
pred <- predict(lm.fit, newdata = test)
mae <- mean(abs(pred - test$Europe))
rmse <- sqrt(mean((pred - test$Europe)^2))
mape <- mean(abs((pred - test$Europe)/pred)) * 100
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAPE = %.2f%%\n", mape))
results <- data.frame(actual = test$Europe, predicted = pred)
ggplot(results, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Europe Price - Actual vs Predicted",
       x = "Actual",
       y = "Predicted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

set.seed(123)
trainIndex <- createDataPartition(crude_oil_prices$NY, p = .7, list = FALSE)
train <- crude_oil_prices[trainIndex, ]
test <- crude_oil_prices[-trainIndex, ]
lm.fit <- lm(NY ~ Year, data = train)
pred <- predict(lm.fit, newdata = test)
mae <- mean(abs(pred - test$NY))
rmse <- sqrt(mean((pred - test$NY)^2))
mape <- mean(abs((pred - test$NY)/pred)) * 100
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAPE = %.2f%%\n", mape))
results <- data.frame(actual = test$NY, predicted = pred)
ggplot(results, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "NY Price - Actual vs Predicted",
       x = "Actual",
       y = "Predicted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

set.seed(123)
trainIndex <- createDataPartition(crude_oil_prices$Gasoline_us, p = .7, list = FALSE)
train <- crude_oil_prices[trainIndex, ]
test <- crude_oil_prices[-trainIndex, ]
lm.fit <- lm(Gasoline_us ~ Year, data = train)
pred <- predict(lm.fit, newdata = test)
mae <- mean(abs(pred - test$Gasoline_us))
rmse <- sqrt(mean((pred - test$Gasoline_us)^2))
mape <- mean(abs((pred - test$Gasoline_us)/pred)) * 100
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAPE = %.2f%%\n", mape))
results <- data.frame(actual = test$Gasoline_us, predicted = pred)
ggplot(results, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Gasoline_us Price - Actual vs Predicted",
       x = "Actual",
       y = "Predicted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

set.seed(123)
trainIndex <- createDataPartition(crude_oil_prices$gasoline_la, p = .7, list = FALSE)
train <- crude_oil_prices[trainIndex, ]
test <- crude_oil_prices[-trainIndex, ]
lm.fit <- lm(gasoline_la ~ Year, data = train)
pred <- predict(lm.fit, newdata = test)
mae <- mean(abs(pred - test$ gasoline_la))
rmse <- sqrt(mean((pred - test$gasoline_la)^2))
mape <- mean(abs((pred - test$gasoline_la)/pred)) * 100
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAPE = %.2f%%\n", mape))
results <- data.frame(actual = test$gasoline_la, predicted = pred)
ggplot(results, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Gasoline_la Price - Actual vs Predicted",
       x = "Actual",
       y = "Predicted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Autocorrelation plot
acf(crude_oil_cushing)
acf(crude_oil_europe)
acf(crude_oil_ny)
acf(crude_oil_gasoline_us)
acf(crude_oil_gasoline_la)

# Partial autocorrelation plot
pacf(crude_oil_cushing)
pacf(crude_oil_europe)
pacf(crude_oil_ny)
pacf(crude_oil_gasoline_us)
pacf(crude_oil_gasoline_la)

# Forecast using ARIMA model and ADF Test
crude_oil_arima <- auto.arima(crude_oil_cushing)
crude_oil_forecast <- forecast(crude_oil_arima, h = 5)  # Forecast for the next 5 years
plot(crude_oil_forecast, main = "Forecast of Cushing Prices using ARIMA")
crude_oil_pred <- crude_oil_forecast$mean
cat("Predicted cushing prices for next 5 years:\n")
cat(crude_oil_pred)
actual <- crude_oil_prices$Cushing[(nrow(crude_oil_prices)-4):nrow(crude_oil_prices)]
rmse <- sqrt(mean((actual - crude_oil_pred)^2))
mae <- mean(abs(actual - crude_oil_pred))
mape <- mean(abs((actual - crude_oil_pred)/actual)) * 100
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("MAPE = %.2f%%\n", mape))
adf_result <- adf.test(crude_oil_cushing)
cat(sprintf("ADF Statistic: %.2f\n", adf_result$statistic))
cat(sprintf("p-value: %.4f\n", adf_result$p.value))
cat(sprintf("Critical values: %.2f %.2f %.2f\n", adf_result$critical[1], adf_result$critical[2], adf_result$critical[3]))
if (adf_result$p.value < 0.05) {
  cat("The time series is stationary.\n")
} else {
  cat("The time series is non-stationary.\n")
}

crude_oil_arima <- auto.arima(crude_oil_europe)
crude_oil_forecast <- forecast(crude_oil_arima, h = 5)  # Forecast for the next 5 years
plot(crude_oil_forecast, main = "Forecast of Europe Brent Spot Prices using ARIMA")
crude_oil_pred <- crude_oil_forecast$mean
cat("Predicted Europe prices for next 5 years:\n")
cat(crude_oil_pred)
actual <- crude_oil_prices$Europe[(nrow(crude_oil_prices)-4):nrow(crude_oil_prices)]
rmse <- sqrt(mean((actual - crude_oil_pred)^2))
mae <- mean(abs(actual - crude_oil_pred))
mape <- mean(abs((actual - crude_oil_pred)/actual)) * 100
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("MAPE = %.2f%%\n", mape))
adf_result <- adf.test(crude_oil_europe)
cat(sprintf("ADF Statistic: %.2f\n", adf_result$statistic))
cat(sprintf("p-value: %.4f\n", adf_result$p.value))
cat(sprintf("Critical values: %.2f %.2f %.2f\n", adf_result$critical[1], adf_result$critical[2], adf_result$critical[3]))
if (adf_result$p.value < 0.05) {
  cat("The time series is stationary.\n")
} else {
  cat("The time series is non-stationary.\n")
}

crude_oil_arima <- auto.arima(crude_oil_ny)
crude_oil_forecast <- forecast(crude_oil_arima, h = 5)  # Forecast for the next 5 years
plot(crude_oil_forecast, main = "Forecast of New York Harbor Conventional Gasoline Regular Spot Price Prices using ARIMA")
crude_oil_pred <- crude_oil_forecast$mean
cat("Predicted NY prices for next 5 years:\n")
cat(crude_oil_pred)
actual <- crude_oil_prices$NY[(nrow(crude_oil_prices)-4):nrow(crude_oil_prices)]
rmse <- sqrt(mean((actual - crude_oil_pred)^2))
mae <- mean(abs(actual - crude_oil_pred))
mape <- mean(abs((actual - crude_oil_pred)/actual)) * 100
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("MAPE = %.2f%%\n", mape))
adf_result <- adf.test(crude_oil_ny)
cat(sprintf("ADF Statistic: %.2f\n", adf_result$statistic))
cat(sprintf("p-value: %.4f\n", adf_result$p.value))
cat(sprintf("Critical values: %.2f %.2f %.2f\n", adf_result$critical[1], adf_result$critical[2], adf_result$critical[3]))
if (adf_result$p.value < 0.05) {
  cat("The time series is stationary.\n")
} else {
  cat("The time series is non-stationary.\n")
}

crude_oil_arima <- auto.arima(crude_oil_gasoline_us)
crude_oil_forecast <- forecast(crude_oil_arima, h = 5)  # Forecast for the next 5 years
plot(crude_oil_forecast, main = "Forecast of U.S. Gulf Coast Conventional Gasoline Regular Spot Price Prices using ARIMA")
crude_oil_pred <- crude_oil_forecast$mean
cat("Predicted Gasoline(US) prices for next 5 years:\n")
cat(crude_oil_pred)
actual <- crude_oil_prices$Gasoline_us[(nrow(crude_oil_prices)-4):nrow(crude_oil_prices)]
rmse <- sqrt(mean((actual - crude_oil_pred)^2))
mae <- mean(abs(actual - crude_oil_pred))
mape <- mean(abs((actual - crude_oil_pred)/actual)) * 100
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("MAPE = %.2f%%\n", mape))
adf_result <- adf.test(crude_oil_gasoline_us)
cat(sprintf("ADF Statistic: %.2f\n", adf_result$statistic))
cat(sprintf("p-value: %.4f\n", adf_result$p.value))
cat(sprintf("Critical values: %.2f %.2f %.2f\n", adf_result$critical[1], adf_result$critical[2], adf_result$critical[3]))
if (adf_result$p.value < 0.05) {
  cat("The time series is stationary.\n")
} else {
  cat("The time series is non-stationary.\n")
}

crude_oil_arima <- auto.arima(crude_oil_gasoline_la)
crude_oil_forecast <- forecast(crude_oil_arima, h = 5)  # Forecast for the next 5 years
plot(crude_oil_forecast, main = "Forecast of Los Angeles Reformulated RBOB Regular Gasoline Spot Price Prices using ARIMA")
crude_oil_pred <- crude_oil_forecast$mean
cat("Predicted Gasoline(LA) prices for next 5 years:\n")
cat(crude_oil_pred)
actual <- crude_oil_prices$gasoline_la[(nrow(crude_oil_prices)-4):nrow(crude_oil_prices)]
rmse <- sqrt(mean((actual - crude_oil_pred)^2))
mae <- mean(abs(actual - crude_oil_pred))
mape <- mean(abs((actual - crude_oil_pred)/actual)) * 100
cat(sprintf("RMSE = %.2f\n", rmse))
cat(sprintf("MAE = %.2f\n", mae))
cat(sprintf("MAPE = %.2f%%\n", mape))
adf_result <- adf.test(crude_oil_gasoline_la)
cat(sprintf("ADF Statistic: %.2f\n", adf_result$statistic))
cat(sprintf("p-value: %.4f\n", adf_result$p.value))
cat(sprintf("Critical values: %.2f %.2f %.2f\n", adf_result$critical[1], adf_result$critical[2], adf_result$critical[3]))
if (adf_result$p.value < 0.05) {
  cat("The time series is stationary.\n")
} else {
  cat("The time series is non-stationary.\n")
}

#Neural Network Model
data <- crude_oil_prices %>% select(Year, Cushing)
set.seed(123)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
nn_model <- neuralnet(Cushing ~ Year, data = train_data, hidden = 3, linear.output = TRUE)
nn_predictions <- predict(nn_model, test_data)
ggplot(data = test_data, aes(x = Year, y = Cushing)) +
  geom_point() +
  geom_line(aes(y = nn_predictions))
results <- data.frame(actual = test$Cushing,
                      predicted = predict(model, newdata = test))
ggplot(results, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Cushing Price", y = "Predicted Cushing Price", 
       title = "Actual vs. Predicted Cushing Prices") 
mae <- mae(test_data$Cushing, nn_predictions) 
rmse <- rmse(test_data$Cushing, nn_predictions)
rsq <- cor(test_data$Cushing, nn_predictions)^2
cat(paste("Mean Absolute Error:", round(mae, 2), "\n"))
cat(paste("Root Mean Squared Error:", round(rmse, 2), "\n"))
cat(paste("R-squared:", round(rsq, 2), "\n"))