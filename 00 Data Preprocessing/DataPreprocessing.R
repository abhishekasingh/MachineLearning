# Data Preprocessing

## Import dataset
dataset = read.csv('Data.csv')

## Taking care of missing data

### For Age
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x,na.rm = TRUE)),
                     dataset$Age)

### For Salary
dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x,na.rm = TRUE)),
                        dataset$Salary)

## Encode Categorial data

dataset$Country = factor(dataset$Country,
                         levels = c('France','Spain','Germany'),
                         labels = c(1,2,3))

dataset$Purchased = factor(dataset$Purchased,
                         levels = c('No','Yes'),
                         labels = c(0,1))

## Splitting the dataset into the training set and test set

library(caTools)
set.seed(123)

### Split the dataset based on dependent variable
split  = sample.split(dataset$Purchased, SplitRatio = 0.8)
split

### Create training set
trainig_set = subset(dataset,split == TRUE)

### Create test set
test_set = subset(dataset,split == FALSE)

## Feature scaling

## Scale the training set
trainig_set[, 2:3] = scale(trainig_set[, 2:3])

## Scale the training set
test_set[, 2:3] = scale(test_set[, 2:3])