# 001 Data Preprocessing

## 1. Import Dataset
df = read.csv("Data.csv")
head(df)
summary(df)

## 2. Missing Values
### Check for missing values in dataframe
sapply(df, function(x) sum(is.na(x)))

### Handle missing values in dataframe
df$Age <- ifelse(is.na(df$Age),
                 ave(df$Age,FUN = function(x) mean(x, na.rm = TRUE)),
                 df$Age)

df$Salary <- ifelse(is.na(df$Salary),
                 ave(df$Salary,FUN = function(x) mean(x, na.rm = TRUE)),
                 df$Salary)

## 3. Encode Categorial Variables
df$Country <- factor(df$Country,
                     levels = c('France','Spain','Germany'),
                     labels = c(1,2,3))

df$Purchased <- factor(df$Purchased,
                     levels = c('No','Yes'),
                     labels = c(0,1))

## 4. Split the dataset into Training set and test set
library(caTools)

set.seed(123)

split = sample.split(df$Purchased, SplitRatio = 0.8)

training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

## 5. Feature Scaling
### For Training Set
training_set[,2:3] = scale(training_set[,2:3])
### For Test Set
test_set[,2:3] = scale(test_set[,2:3])