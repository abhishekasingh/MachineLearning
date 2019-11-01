# 001 Multiple Linear Regression

## 1. Import the dataset

df = read.csv('50_Startups.csv')
head(df)

## 2. Encoding categorical data
df$State = factor(df$State,
                  levels = c('New York', 'California', 'Florida'),
                  labels = c(1, 2, 3))

## 3. Split the dataset into Training set and test set

library(caTools)
set.seed(123)
split = sample.split(df$Profit, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

## 4. Fit Multiple Linear Regression to the Training Set

regressor = lm(formula = Profit ~ .,
               data = training_set)
summary(regressor)

## 5. Predict the Test set results

y_predict = predict(regressor, newdata = test_set)
y_predict

## 6. Backward Elimination

#colnames(df)

# Fit the model with all possible predictors
regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = df)
summary(regressor)

# Consider the predictor with the highest p-value . If P > SL, remove the predictor

regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
                data = df)
summary(regressor)

regressor <- lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
                data = df)
summary(regressor)

regressor <- lm(formula = Profit ~ R.D.Spend,
                data = df)
summary(regressor)

## Backward Elimination

backwardElimination <- function(x, sl) {
    numVars = length(x)
    for (i in c(1:numVars)){
        regressor = lm(formula = Profit ~ ., data = x)
        maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
        if (maxVar > sl){
            j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
            x = x[, -j]
        }
        numVars = numVars - 1
    }
    return(summary(regressor))
}

SL = 0.05
df = df[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)