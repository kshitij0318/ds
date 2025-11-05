# DS LAB - Multiple Regression
# Dataset: supply_chain.csv

getwd()
setwd("C:/OneDrive/Desktop/Coding projects/dsl")

install.packages("ggplot2")
library(ggplot2)

data <- read.csv("supply_chain.csv", stringsAsFactors = FALSE)

head(data)
str(data)

# 1 Multiple Regression

model <- lm(Revenue.generated ~ Price + Manufacturing.costs, data = data)
summary(model)

# 2. Underfitting vs Overfitting Check

# R-squared and Adjusted R-squared
cat("R-squared:", summary(model)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model)$adj.r.squared, "\n")

# If R-squared is very low → underfitting
# If R-squared is very high but test accuracy drops → overfitting

# 3. Model Accuracy & Error Metrics
# Predictions
data$Predicted <- predict(model, data)

# Residuals (errors)
data$Residuals <- data$Revenue.generated - data$Predicted
mae <- mean(abs(data$Residuals))
mse <- mean((data$Residuals)^2)
rmse <- sqrt(mse)
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

# Visualization

# Actual vs Predicted Plot
ggplot(data, aes(x = Revenue.generated, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Revenue",
       x = "Actual Revenue", y = "Predicted Revenue")

# Residual Plot
ggplot(data, aes(x = Predicted, y = Residuals)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals")
