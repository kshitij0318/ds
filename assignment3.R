getwd()
setwd("C:/OneDrive/Desktop/Coding projects/dsl")

# Load required libraries
library(utils)
install.packages("readxl")
library(readxl)
install.packages("xml2")
library(xml2)
install.packages("XML")
library("XML")
library("methods")

# === Excel Section ===
# Read Excel files
Mall_Customers <- read_excel("Mall Customers.xlsx")
Supply_Chain <- read_excel("supply_chain.xlsx")

# Read specific sheets from Excel workbook
my_excel_sheet2 <- read_excel("Mall Customers.xlsx", sheet = "test1")
head(my_excel_sheet2)
my_excel_sheet3 <- read_excel("Mall Customers.xlsx", sheet = 3)
head(my_excel_sheet3)

# Check if Mall_Customers is a data frame
print(is.data.frame(Mall_Customers))

# Number of columns in the Excel data set
print(ncol(Mall_Customers))
# Number of rows in the Excel data set
print(nrow(Mall_Customers))

# Find product with maximum revenue generated from Supply_Chain
product_max_revenue <- Supply_Chain[which.max(Supply_Chain$`Revenue generated`), ]
print("Product with Maximum Revenue Generated:")
print(product_max_revenue)

# First few rows of the Supply_Chain data set
head(Supply_Chain)
# Get a summary of each column
summary(Supply_Chain)
# Check the structure of the data frame 
str(Supply_Chain)
# Names of the columns
colnames(Supply_Chain)
# Number of rows and columns
dim(Supply_Chain)

# === CSV Section ===
# Read CSV files
LoanTrain_csv <- read.csv("loan-train.csv")

# === XML Section ===
# Read XML files
Sample_xml <- read_xml("sample.xml")
# Parse and print raw XML content using XML package
result<-xmlParse(file="sample.xml")
result