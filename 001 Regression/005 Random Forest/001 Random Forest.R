# Random Forest

## 1. Import the dataset
df = read.csv('Position_Salaries.csv')
df = df[2:3]

## 2. Fit Random Forest
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor = randomForest(x = df[-2],
                         y = df$Salary,
                         ntree = 500)

## 3. Predict a new result
y_pred = predict(regressor, data.frame(Level = 6.5))
y_pred

## 4. Visualising the Random Forest Regression results (higher resolution)
library(ggplot2)
x_grid = seq(min(df$Level), max(df$Level), 0.01)
ggplot() +
    geom_point(aes(x = df$Level, y = df$Salary),
               colour = 'red') +
    geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
              colour = 'blue') +
    ggtitle('Truth or Bluff (Random Forest Regression)') +
    xlab('Level') +
    ylab('Salary')