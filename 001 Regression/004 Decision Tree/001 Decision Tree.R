# Decision Tree

#install.packages('rpart')
library(rpart)

## 1. Import dataset
df <- read.csv('Position_Salaries.csv')
df = df[2:3]

## 2. Fit a model
regressor <- rpart(formula = Salary ~ .,
                   data = df,
                   control = rpart.control(minsplit = 1))
summary(regressor)

## 3. Predict a new result
y_pred = predict(regressor, data.frame(Level = 6.5))
y_pred

# Visualise the Decision Tree Regression results (higher resolution)
library(ggplot2)
x_grid = seq(min(df$Level), max(df$Level), 0.01)
ggplot() +
    geom_point(aes(x = df$Level, y = df$Salary),
               colour = 'red') +
    geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
              colour = 'blue') +
    ggtitle('Truth or Bluff (Decision Tree Regression)') +
    xlab('Level') +
    ylab('Salary')

# Plotting the tree
plot(regressor)
text(regressor)