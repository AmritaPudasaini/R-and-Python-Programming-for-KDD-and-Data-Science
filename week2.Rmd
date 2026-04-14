---
title: "week2"
author: "AmritaPudasaini"
date: "2026-03-17"
output: html_document
---

```{r}
data <- read.csv("C:/Users/DELL/Downloads/cereal_1.csv")
data$rating <- as.numeric(as.character(data$rating))
data$fiber  <- as.numeric(as.character(data$fiber))
data <- na.omit(data)
```

```{r}
model1 <- lm(rating ~ fiber, data = data)
summary(model1)
```

```{r}
summary(model1)$sigma
```

```{r}
summary(model1)$r.squared
```

```{r}
predict(model1, newdata = data.frame(fiber = 3))
```

```{r}
predict(model1, newdata = data.frame(fiber = 3), interval = "confidence")
```

```{r}
predict(model1, newdata = data.frame(fiber = 3), interval = "prediction")
```

```{r}
# Basic scatter plot
plot(data$fiber, data$rating,
     main = "Scatter Plot of Rating vs Fiber",
     xlab = "Fiber (grams)",
     ylab = "Rating",
     pch = 19)

# Add regression line
abline(model1, lwd = 2)
```
#a
```{r}
model2 <- lm(rating ~ fiber + sugars, data = data)
summary(model2)
```

```{r}
summary(model1)$r.squared
summary(model2)$r.squared
```

```{r}
summary(model1)$sigma
summary(model2)$sigma
```

```{r}

```

```{r}

```