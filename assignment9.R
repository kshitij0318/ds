# Lab - Decision Tree
# Dataset: supply_chain.csv

install.packages(c("tidyverse", "caret", "rpart", "rpart.plot"))
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

setwd("C:/OneDrive/Desktop/Coding projects/dsl")

data <- read.csv("supply_chain.csv", stringsAsFactors = FALSE) %>% na.omit()

# Convert target variable to binary classes based on median Revenue
threshold <- median(data$Revenue.generated)
data$RevenueClass <- factor(ifelse(data$Revenue.generated > threshold, "High", "Low"))

# Train-test split (80-20)
set.seed(123)
trainIndex <- createDataPartition(data$RevenueClass, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test  <- data[-trainIndex, ]

# Train Decision Tree model
model <- rpart(RevenueClass ~ Price + Manufacturing.costs, data = train, method = "class")

# Predictions & Evaluation
pred <- predict(model, test, type = "class")
cm <- confusionMatrix(pred, test$RevenueClass)
print(cm)

# Metrics
acc <- cm$overall['Accuracy']
prec <- cm$byClass['Precision']
rec <- cm$byClass['Recall']
f1 <- 2 * (prec * rec) / (prec + rec)
cat("\n--- Model Performance ---\n")
cat("Accuracy :", round(acc, 3), "\n")
cat("Precision:", round(prec, 3), "\n")
cat("Recall   :", round(rec, 3), "\n")
cat("F1-score :", round(f1, 3), "\n")

# Plot Decision Tree
rpart.plot(model, main = "Decision Tree Structure")

# Decision Boundary Visualization (Price vs Manufacturing.costs)
x_seq <- seq(min(data$Price), max(data$Price), length.out = 200)
y_seq <- seq(min(data$Manufacturing.costs), max(data$Manufacturing.costs), length.out = 200)
grid <- expand.grid(Price = x_seq, Manufacturing.costs = y_seq)
grid$Pred <- predict(model, grid, type = "class")

ggplot() +
  geom_tile(data = grid, aes(Price, Manufacturing.costs, fill = Pred), alpha = 0.4) +
  geom_point(data = data, aes(Price, Manufacturing.costs, color = RevenueClass), size = 2) +
  scale_fill_manual(values = c("High" = "blue", "Low" = "red")) +
  scale_color_manual(values = c("High" = "blue", "Low" = "red")) +
  labs(title = "Decision Boundary - Decision Tree", x = "Price", y = "Manufacturing Costs") +
  theme_minimal()
