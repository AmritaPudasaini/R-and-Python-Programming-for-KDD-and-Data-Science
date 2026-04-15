---
title: "week1"
author: "AmritaPudasaini"
date: "2026-03-08"
output: html_document
---

```{r}
data <- c(10,15,20,25,30,35,40,45,50,50)

```
#Mean
```{r}
mean_val <- mean(data)
print(paste("Mean: ", mean_val))
```
#Meadian
```{r}
median_val<-median(data)
print(paste("Median: ", median_val))
```
#Mode (most frequent value)
```{r}
mode_val <- names(sort(table(data), decreasing = TRUE)) [1]
print(paste("Mode: ", mode_val))
```
#Summary
```{r}
summary(data)
```
#Histogram
```{r}
hist(data, 
     main="Histogram", 
     xlab="Value", 
     ylab="Frequency", 
     col="lightblue", 
     border="black")
```
#Scatter Plot
```{r}
plot(data, 
     main="Scatter Plot", 
     xlab="Index", 
     ylab="Value", 
     pch=19, 
     col="blue")
```
#BoxPlot
```{r}
boxplot(data, 
        main="Boxplot", 
        ylab="Value", 
        col="lightgreen")
```
#Pie chart
```{r}
min_max_norm <- (data -min(data)) / (max(data) - min(data))
print(min_max_norm)
```
