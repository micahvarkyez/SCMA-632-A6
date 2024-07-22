# Load necessary libraries and packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("forecast")
install.packages("tseries")
install.packages("caret")
install.packages("zoo")
install.packages("timetk")

library(ggplot2)
library(dplyr)
library(forecast)
library(tseries)
library(caret)
library(zoo)
library(timetk)

# Load the data
data <- read.csv("C:\\Users\\Dell\\Desktop\\MICAH\\Mahindra.csv")

# Display the first few rows of the data
head(data)

# Convert Date to Date type if not already
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Check for missing values
sum(is.na(data))

# Check for outliers using boxplot
boxplot(data$Adj.Close, main="Boxplot for Adj Close", col="lightblue")

# Remove outliers if necessary 
Q1 <- quantile(data$Adj.Close, 0.25)
Q3 <- quantile(data$Adj.Close, 0.75)
IQR <- Q3 - Q1

data <- data %>% filter(Adj.Close > (Q1 - 1.5 * IQR) & Adj.Close < (Q3 + 1.5 * IQR))

# Plot the Adj Close Price
ggplot(data, aes(x = Date, y = Adj.Close)) +
  geom_line(color = 'blue') +
  labs(title = 'M&M.NS Adj Close Price', x = 'Date', y = 'Adj Close Price') +
  theme_minimal()

# Split the data into train and test sets
set.seed(123)
trainIndex <- createDataPartition(data$Adj.Close, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Convert to monthly data
monthly_data <- data %>%
  group_by(Date = floor_date(Date, "month")) %>%
  summarize(Adj.Close = mean(Adj.Close))

# Convert to time series
ts_monthly <- ts(monthly_data$Adj.Close, frequency = 12, start = c(year(min(data$Date)), month(min(data$Date))))

# Decompose using additive model
decomp_add <- decompose(ts_monthly, type = "additive")

# Decompose using multiplicative model
decomp_mult <- decompose(ts_monthly, type = "multiplicative")

# Plot decompositions
plot(decomp_add)
plot(decomp_mult)

# Fit Holt Winters model
hw_model <- HoltWinters(ts_monthly)

# Forecast for the next year
hw_forecast <- forecast(hw_model, h = 12)
plot(hw_forecast)

# Fit ARIMA model
arima_model <- auto.arima(data$Adj.Close)
summary(arima_model)

# Diagnostic check
tsdiag(arima_model)

# Forecast for the next three months
arima_forecast <- forecast(arima_model, h = 90)
plot(arima_forecast)

# Fit SARIMA model if seasonality is present
sarima_model <- auto.arima(data$Adj.Close, seasonal = TRUE)
summary(sarima_model)

# Forecast with SARIMA
sarima_forecast <- forecast(sarima_model, h = 90)
plot(sarima_forecast)

# Fit ARIMA to monthly series
arima_monthly <- auto.arima(ts_monthly)
summary(arima_monthly)

# LSTM model can be implemented using keras or tensorflow package
library(keras)

# Define and train your LSTM model here
# Note: Requires significant setup and customization

# Random Forest and Decision Tree require more features; example below assumes feature engineering is done

# Load randomForest library
library(randomForest)

rf_model <- randomForest(Close ~ ., data = train_data)
rf_predictions <- predict(rf_model, test_data)
plot(test_data$Date, test_data$Close, type = "l", col = "blue")
lines(test_data$Date, rf_predictions, col = "red")  

tree_model <- rpart(Close ~ ., data = train_data)
tree_predictions <- predict(tree_model, test_data)
plot(test_data$Date, test_data$Close, type = "l", col = "blue")
lines(test_data$Date, tree_predictions, col = "red")



