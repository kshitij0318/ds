install.packages("corrplot")
library(corrplot)
data(iris)

# 1. Correlation Matrix
iris_numeric <- iris[, 1:4]

cor_matrix <- cor(iris_numeric)
print(cor_matrix)

#2.Correlation plot
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", 
         number.cex = 0.7)

# 3. Analysis of Covariance (ANOVA)
ancova_model <- aov(Sepal.Length ~ Species + Sepal.Width, data = iris)
summary(ancova_model)







# 1. Read the data set
getwd()
setwd("C:/OneDrive/Desktop/Coding projects/dsl")
install.packages("readxl")
install.packages("ggplot2")
library(readxl)
library(ggplot2)
data <- read_excel("supply_chain.xlsx", sheet = "supply_chain")
head(data)
str(data)
# Linear Regression
model <- lm(`Revenue generated` ~ Price, data = data)
summary(model)
# Get predicted values and residuals (deviation)
data$Predicted <- predict(model, data)
data$Residuals <- data$`Revenue generated` - data$Predicted # residuals
head(data[, c("Revenue generated", "Predicted", "Residuals")])
# regression plot
ggplot(data, aes(x = Price, y = `Revenue generated`)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Linear Regression: Revenue generated vs Price", x = "Price", y = "Revenue generated")


