---
title: "week4"
author: "AmritaPudasaini"
date: "2026-04-03"
output: html_document
---

```{r}
# Load libraries
library(rpart)
library(rpart.plot)
library(C50)
library(caret)
library(dplyr)

# Load dataset
data <- read.csv("C:/Users/DELL/Downloads/churn-bigml-20.csv")
data

# Convert target variable
data$Churn <- as.factor(data$Churn)

# Normalize numerical variables
num_vars <- sapply(data, is.numeric)
data[num_vars] <- scale(data[num_vars])

# Remove highly correlated variables
cor_matrix <- cor(data[num_vars])
high_corr <- findCorrelation(cor_matrix, cutoff = 0.8)
data <- data[, -which(names(data) %in% names(data[num_vars])[high_corr])]
```

```{r}
cart_model <- rpart(Churn ~ ., data = data, method = "class")
rpart.plot(cart_model)
```

```{r}
c45_model <- C5.0(Churn ~ ., data = data)
summary(c45_model)
plot(c45_model)
```

```{r}
install.packages("rpart.plot")
install.packages("rpart.rules")
library(rpart.plot)
rpart.rules(cart_model)
```

```{r}
library(C50)
rules_c45 <- C5.0(Churn ~ ., data = data, rules = TRUE)
summary(rules_c45)
```

```{r}
library(arules)

# Load dataset
train <- read.csv("C:/Users/DELL/Downloads/churn-bigml-20.csv")
# Clean column names
colnames(train) <- make.names(colnames(train))

# Select required variables (CORRECTED)
train <- train[, c("Voice.mail.plan", 
                   "International.plan", 
                   "Customer.service.calls", 
                   "Churn")]

# Convert all to factors
train[] <- lapply(train, as.factor)

# Convert Customer service calls to ordinal
train$Customer.service.calls <- ordered(train$Customer.service.calls)

# Convert to transactions
trans <- as(train, "transactions")
trans
```

```{r}
unique(train$Churn)
```

```{r}
install.packages("apriori")
library(arules)
unique(train$Churn)
rules_a <- apriori(
  trans,
  parameter = list(supp = 0.01, conf = 0.05, maxlen = 2),
  appearance = list(
    rhs = c("Churn=Yes", "Churn=No"),
    default = "lhs"
  )
)

inspect(rules_a)
rules_a
```


```{r}
rules_sorted <- sort(rules_a, by = "lift", decreasing = TRUE)
best_rule <- rules_sorted[1]

inspect(best_rule)
```

```{r}

```

```{r}

```

```{r}

```

