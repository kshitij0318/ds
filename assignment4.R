getwd()
setwd("C:/OneDrive/Desktop/Coding projects/dsl")
install.packages("ggplot2")
library(ggplot2)
library(readxl)

# Load data
data <- read_excel("Mall Customers.xlsx", sheet = "Sheet1")

# (a) Box plot & Scatter plot
boxplot(data$Age, data$`Annual Income (k$)`, data$`Spending Score (1-100)`,
        names = c("Age", "Income", "Spending"),
        main = "Boxplots of Customer Features",
        col = c("lightblue", "lightgreen", "lightpink"))

plot(data$Age, data$`Spending Score (1-100)`,
     main = "Age vs Spending Score",
     xlab = "Age", ylab = "Spending Score",
     col = "blue", pch = 19)

plot(data$`Annual Income (k$)`, data$`Spending Score (1-100)`,
     main = "Income vs Spending Score",
     xlab = "Income (k$)", ylab = "Spending Score",
     col = "darkgreen", pch = 19)

# (b) Outliers
boxplot(data$`Annual Income (k$)`, main = "Outliers in Income", col = "orange")
boxplot(data$`Spending Score (1-100)`, main = "Outliers in Spending Score", col = "purple")

# (c) Histogram, Bar, Pie
hist(data$Age, main = "Histogram of Age", xlab = "Age",
     col = "lightblue", border = "black")

# Bar chart: Count of Marital Status
barplot(table(data$`Marital Status`),
        main = "Bar Chart of Marital Status",
        col = c("tomato", "skyblue", "lightgreen"),
        ylab = "Count")

gender_count <- table(data$Gender)
pie(gender_count, labels = paste(names(gender_count), gender_count),
    main = "Gender Distribution",
    col = c("pink", "lightblue"))
