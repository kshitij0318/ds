# DS LAB - Multiple Regression & Evaluation
# Dataset: supply_chain.csv
install.packages("tidyverse")
install.packages(c("ggplot2", "dplyr", "caret", "glmnet", "infotheo", "reshape2"))
library(ggplot2)
library(dplyr)
library(caret)
library(glmnet)
library(infotheo)
library(reshape2)

setwd("C:/OneDrive/Desktop/Coding projects/dsl")

# Load dataset
data <- read.csv("supply_chain.csv", stringsAsFactors = FALSE)
data <- na.omit(data)
str(data)
summary(data)

# Multiple Regression Model
model <- lm(Revenue.generated ~ Price + Manufacturing.costs, data = data)
summary(model)

# Add predictions
data$Predicted <- predict(model, data)
data$Residuals <- data$Revenue.generated - data$Predicted

# Model Performance Metrics
r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared

mae <- mean(abs(data$Residuals))
mse <- mean((data$Residuals)^2)
rmse <- sqrt(mse)

cat("--- Model Performance ---\n")
cat("R-squared:", round(r_squared, 4), "\n")
cat("Adjusted R-squared:", round(adj_r_squared, 4), "\n")
cat("MAE:", round(mae, 4), "\n")
cat("MSE:", round(mse, 4), "\n")
cat("RMSE:", round(rmse, 4), "\n\n")

# Descriptive Statistics

mean_rev <- mean(data$Revenue.generated)
median_rev <- median(data$Revenue.generated)
mode_rev <- as.numeric(names(sort(table(data$Revenue.generated), decreasing = TRUE))[1])
iqr_rev <- IQR(data$Revenue.generated)

cat("--- Descriptive Statistics for Revenue ---\n")
cat("Mean:", mean_rev, "\n")
cat("Median:", median_rev, "\n")
cat("Mode:", mode_rev, "\n")
cat("Interquartile Range (IQR):", iqr_rev, "\n\n")

# Classification Metrics
# Convert regression output to classification based on median threshold
threshold <- median(data$Revenue.generated)
actual_class <- ifelse(data$Revenue.generated > threshold, 1, 0)
predicted_class <- ifelse(data$Predicted > threshold, 1, 0)

conf_mat <- confusionMatrix(as.factor(predicted_class), as.factor(actual_class))
precision <- conf_mat$byClass['Precision']
recall <- conf_mat$byClass['Recall']
accuracy <- conf_mat$overall['Accuracy']

cat("--- Classification Metrics ---\n")
cat("Accuracy:", round(accuracy, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall:", round(recall, 4), "\n\n")

# Information Theory Metrics
# Discretize revenue for entropy & info gain
data$Revenue_Bin <- discretize(data$Revenue.generated, disc = "equalfreq", nbins = 3)

# Check if 'Product.type' or similar categorical variable exists
categorical_cols <- names(Filter(is.character, data))
if (length(categorical_cols) > 0) {
  cat_col <- categorical_cols[1]  # Use first categorical column
  entropy_revenue <- entropy(table(data$Revenue_Bin))
  entropy_cat <- entropy(table(data[[cat_col]]))
  info_gain <- mutinformation(data[[cat_col]], data$Revenue_Bin)
  
  cat("--- Information Theory Metrics ---\n")
  cat("Entropy of Revenue:", round(entropy_revenue, 4), "\n")
  cat("Entropy of", cat_col, ":", round(entropy_cat, 4), "\n")
  cat("Information Gain (", cat_col, "→ Revenue):", round(info_gain, 4), "\n\n")
}


# Model Visualization

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

# Correlation Heatmap
cor_matrix <- data %>%
  select(Revenue.generated, Price, Manufacturing.costs) %>%
  cor()

library(reshape2)
melted_cor <- melt(cor_matrix)
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal()

# Interpretation Notes
cat("INTERPRETATION NOTES:\n")
cat("1. Multiple regression reveals how Price and Manufacturing Costs affect Revenue.\n")
cat("2. Compare R-squared and Adjusted R-squared to detect underfitting/overfitting.\n")
cat("3. Error metrics (MAE, RMSE) show prediction accuracy.\n")
cat("4. Precision, Recall, and Accuracy evaluate classification reliability.\n")
cat("5. Entropy & Information Gain measure uncertainty and the relevance of categorical variables.\n")
