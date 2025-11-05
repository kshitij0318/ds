# DS LAB-6 A
# Sampling, Covariance, Correlation, Deviation & Regression
# Dataset: Customer Churn

install.packages("caTools")
install.packages("ggplot2")
library(caTools)
library(ggplot2)

getwd()
setwd("C:/OneDrive/Desktop/Coding projects/dsl")
customer_churn <- read.csv(
  "https://raw.githubusercontent.com/IBM/telco-customer-churn-on-icp4d/master/data/Telco-Customer-Churn.csv",
  stringsAsFactors = FALSE
)


head(customer_churn)
str(customer_churn)

set.seed(123)
split <- sample.split(customer_churn$Churn, SplitRatio = 0.7)
train <- subset(customer_churn, split == TRUE)
test  <- subset(customer_churn, split == FALSE)

cat("Training set size:", nrow(train), "\n")
cat("Testing set size:", nrow(test), "\n")

num_data <- customer_churn[, sapply(customer_churn, is.numeric)]

cov_matrix <- cov(num_data, use = "complete.obs")
cor_matrix <- cor(num_data, use = "complete.obs")

cat("Covariance Matrix:\n")
print(cov_matrix)

cat("Correlation Matrix:\n")
print(cor_matrix)

customer_churn$Mean_Revenue <- mean(customer_churn$MonthlyCharges, na.rm = TRUE)
customer_churn$Deviation <- customer_churn$MonthlyCharges - customer_churn$Mean_Revenue

head(customer_churn[, c("MonthlyCharges", "Mean_Revenue", "Deviation")])

model <- lm(MonthlyCharges ~ tenure, data = customer_churn)
summary(model)

ggplot(customer_churn, aes(x = tenure, y = MonthlyCharges)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Linear Regression: MonthlyCharges vs Tenure",
    x = "Tenure",
    y = "Monthly Charges"
  )
