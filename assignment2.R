library(readxl)
mall_customers <- read_excel("Mall Customers.xlsx")

# Assuming the data is already loaded as 'mall_customers'
summary(mall_customers)

# Dimensions of the dataset
dim(mall_customers)

# Quantiles for Annual Income
quantile(mall_customers$`Annual Income (k$)`)

# Mean and median of Annual Income
mean(mall_customers$`Annual Income (k$)`)
median(mall_customers$`Annual Income (k$)`)

# Frequency table for Annual Income
table(mall_customers$`Annual Income (k$)`)

# Structure of the dataset
str(mall_customers)

# Quantiles for Spending Score
quantile(mall_customers$`Spending Score (1-100)`)

# Subset of customers with Annual Income greater than 50
customers_subset <- subset(mall_customers, `Annual Income (k$)` > 50)
print(customers_subset)

# Aggregate mean of numeric columns by Gender
aggregate(cbind(`Annual Income (k$)`, `Spending Score (1-100)`) ~ Gender, data = mall_customers, mean)