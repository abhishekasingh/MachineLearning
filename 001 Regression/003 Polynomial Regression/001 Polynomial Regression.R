# Polynomial Regression

## 1. Import dataset
df <- read.csv('Position_Salaries.csv')
df = df[2:3]

## 2. Fit Linear Regression Model
linear_reg = lm(formula = Salary ~ ., 
                data = df)

summary(linear_reg)

## 3. Fit Polynomial Linear Regression Model
df$Level2 = df$Level^2
df$Level3 = df$Level^3
df$Level4 = df$Level^4
poly_reg = lm(formula = Salary ~ .,
              data = df)

## 4. Visualize Linear Regression results
library(ggplot2)

linear_plt <- ggplot() +
    geom_point(aes(x = df$Level, y = df$Salary),colour = 'red') +
    geom_line(aes(x = df$Level, y = predict(linear_reg, newdata = df)), colour = 'blue') +
    ggtitle('Truth or Bluff (Linear Regression)') +
    xlab('Level') +
    ylab('Salary')
print(linear_plt)

## 5. Visualize Polynomial Linear Regression results
poly_plt <- ggplot() +
    geom_point(aes(x = df$Level, y = df$Salary), colour = 'red') +
    geom_line(aes(x = df$Level, y = predict(poly_reg, newdata = df)), colour = 'blue') +
    ggtitle('Truth or Bluff (Polynomial Regression)') +
    xlab('Level') +
    ylab('Salary')
print(poly_plt)

## 6. Visualising the Regression Model results (for higher resolution and smoother curve)

x_grid = seq(min(df$Level), max(df$Level), 0.1)
res_plt <- ggplot() +
    geom_point(aes(x = df$Level, y = df$Salary), colour = 'red') +
    geom_line(aes(x = x_grid, y = predict(poly_reg,
                                          newdata = data.frame(Level = x_grid,
                                                               Level2 = x_grid^2,
                                                               Level3 = x_grid^3,
                                                               Level4 = x_grid^4))),
              colour = 'blue') +
    ggtitle('Truth or Bluff (Polynomial Regression)') +
    xlab('Level') +
    ylab('Salary')

print(res_plt)

# Predicting a new result with Linear Regression
predict(linear_reg,
        data.frame(Level = 6.5))

# Predicting a new result with Polynomial Regression
predict(poly_reg, data.frame(Level = 6.5,
                             Level2 = 6.5^2,
                             Level3 = 6.5^3,
                             Level4 = 6.5^4))