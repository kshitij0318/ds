# DS LAB-6 B
# Logistic Regression on Iris Dataset

iris_data <- read.csv("https://raw.githubusercontent.com/uiuc-cse/data-fa14/gh-pages/data/iris.csv")
head(iris_data)
str(iris_data)

# Simplify problem: binary classification (Setosa vs Non-Setosa)
iris_data$BinarySpecies <- ifelse(iris_data$species == "setosa", 1, 0)

log_model <- glm(BinarySpecies ~ petal_length, data = iris_data, family = "binomial")
summary(log_model)

# Predictions
iris_data$Predicted <- predict(log_model, iris_data, type = "response")

# Calculate MAE and MSE
mae <- mean(abs(iris_data$BinarySpecies - iris_data$Predicted))
mse <- mean((iris_data$BinarySpecies - iris_data$Predicted)^2)

print(paste("Mean Absolute Error (MAE):", round(mae, 4)))
print(paste("Mean Squared Error (MSE):", round(mse, 4)))

# Check model fit (using null deviance vs residual deviance)
print(paste("Null Deviance:", log_model$null.deviance))
print(paste("Residual Deviance:", log_model$deviance))
