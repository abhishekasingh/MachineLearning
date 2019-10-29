# 001 Simple Linear Regression

## 1. Import Dataset
df = read.csv("Salary_Data.csv")
head(df)
summary(df)

## 2. Missing Values
### Check for missing values in dataframe
sapply(df, function(x) sum(is.na(x)))

## 3. Split the dataset into Training set and test set
library(caTools)

set.seed(123)

split = sample.split(df$Salary, SplitRatio = 2/3)

training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

## 4. Fit Simple Linear Regression to the Training Set
regressor <- lm(formula = Salary ~ YearsExperience,
                data = training_set)

summary(regressor)

## 5. Predict the Test set results
y_pred = predict(regressor, newdata = test_set)

## 6. Visualize the Training set
library(ggplot2)

ggplot() +
    geom_point(aes(x = training_set$YearsExperience, y =training_set$Salary),
               colour = "red") +
    geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
              colour = "blue") +
    ggtitle("Salary vs Experience (Training Set)") +
    xlab("Years of Experience") +
    ylab("Salary")

## 6. Visualize the Test set
library(ggplot2)

ggplot() +
    geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
               colour = "red") +
    geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
              colour = "blue") +
    ggtitle("Salary vs Experience (Test Set)") +
    xlab("Years of Experience") +
    ylab("Salary")