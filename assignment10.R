# Minimal Clustering Lab - K-Means & Hierarchical
# Dataset: supply_chain.csv

install.packages(c("tidyverse", "factoextra"))
library(tidyverse)
library(factoextra)

# Set working directory
setwd("C:/OneDrive/Desktop/Coding projects/dsl")

# Load and clean dataset
data <- read.csv("supply_chain.csv", stringsAsFactors = FALSE) %>% na.omit()

# Select numeric features for clustering
num_data <- data %>% select(Price, Manufacturing.costs, Revenue.generated)

# 1. K-MEANS CLUSTERING

# Elbow Method to find optimal number of clusters (k)
fviz_nbclust(num_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k")

# Apply K-means with k = 3 (example, based on elbow curve)
set.seed(123)
k3 <- kmeans(num_data, centers = 3, nstart = 25)

# Visualize clusters
fviz_cluster(k3, data = num_data, geom = "point", ellipse.type = "norm") +
  labs(title = "K-Means Clustering (k = 3)")

# Add cluster labels back to data
data$Cluster_KMeans <- as.factor(k3$cluster)

# 2. HIERARCHICAL CLUSTERING
# Compute distance matrix and apply hierarchical clustering
dist_matrix <- dist(num_data, method = "euclidean")
hclust_model <- hclust(dist_matrix, method = "ward.D2")

# Dendrogram
plot(hclust_model, labels = FALSE, main = "Hierarchical Clustering Dendrogram")
rect.hclust(hclust_model, k = 3, border = "red")  # Cut into 3 clusters

# Assign clusters to data
data$Cluster_HC <- as.factor(cutree(hclust_model, k = 3))

# 3. Cluster Characteristics
cat("\n--- K-Means Cluster Characteristics ---\n")
print(aggregate(num_data, by = list(Cluster = data$Cluster_KMeans), mean))

cat("\n--- Hierarchical Cluster Characteristics ---\n")
print(aggregate(num_data, by = list(Cluster = data$Cluster_HC), mean))

# 4. Experiment with Different k values for K-Means
for (k in 2:5) {
  set.seed(123)
  km <- kmeans(num_data, centers = k, nstart = 25)
  cat("\n--- K =", k, "---\n")
  cat("Total Within Sum of Squares:", round(km$tot.withinss, 2), "\n")
}

