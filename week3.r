---
title: "week3"
author: "AmritaPudasaini"
date: "2026-03-28"
output: html_document
---
#Load the dataset
```{r}
risk <- read.csv("C:/Users/DELL/Downloads/ClassifyRisk.csv")
risk
```

```{r}
# Convert categorical variable to numeric
risk$marital_status <- as.numeric(as.factor(risk$marital_status))
# Select predictors
X <- risk[, c("age", "marital_status", "income")]
# Compute Euclidean distance matrix
dist_matrix <- as.matrix(dist(X, method = "euclidean"))
# Extract distances from record #1
dist_record1 <- dist_matrix[1, ]
# Sort distances and get nearest neighbors (excluding itself)
sorted_indices <- order(dist_record1)
nearest_neighbors <- sorted_indices[2:3]
# Output
nearest_neighbors
dist_record1[nearest_neighbors]
```
#Load dataset
```{r}
cereal <- read.csv("C:/Users/DELL/Downloads/cereal.csv", stringsAsFactors = FALSE)
cereal
```
#Step1: Normalize Data
```{r}
# Load dataset
cereal <- read.csv("C:/Users/DELL/Downloads/cereal.csv", stringsAsFactors = FALSE)

# Convert columns to numeric
cols_to_convert <- c("calories", "protein", "fat", "sodium",
                     "fiber", "carbo", "sugars", "potass",
                     "vitamins", "shelf", "weight", "cups", "rating")

cereal[cols_to_convert] <- lapply(cereal[cols_to_convert], as.numeric)

# Remove name and rating
cereal_clean <- cereal[, !(names(cereal) %in% c("name", "rating"))]

# Keep only numeric columns
cereal_num <- cereal_clean[, sapply(cereal_clean, is.numeric)]

# Remove NA rows (IMPORTANT)
cereal_num <- na.omit(cereal_num)

# ALSO filter original dataset to match same rows
cereal_final <- cereal[as.numeric(rownames(cereal_num)), ]

# Normalize
cereal_scaled <- scale(cereal_num)
```
#K-means Clustering with k = 5
```{r}
set.seed(123)
k5 <- kmeans(cereal_scaled, centers = 5, nstart = 25)
k5
# Add cluster labels (only for remaining rows)
cereal_clean <- cereal_clean[complete.cases(cereal_clean), ]
cereal_clean$cluster5 <- k5$cluster
cereal_clean
```
# Cluster Profiles for k = 5
```{r}
aggregate(cereal_final[, cols_to_convert],
          by = list(cluster = cereal_final$cluster5),
          mean)
```

# K-means Clustering with k = 3
```{r}
set.seed(123)
k3 <- kmeans(cereal_scaled, centers = 3, nstart = 25)
k3
cereal_final$cluster3 <- k3$cluster
cereal_final
```

# Cluster Profiles for k = 3
```{r}
aggregate(cereal_final[, cols_to_convert],
          by = list(cluster = cereal_final$cluster3),
          mean)
```

# Predicting Ratings Using Cluster Membership
```{r}
library(ggplot2)

ggplot(cereal_final, aes(x = rating, fill = factor(cluster3))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 10) +
  labs(fill = "Cluster", title = "Ratings by Cluster")
```
