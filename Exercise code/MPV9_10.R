# Exercise MPV 9.10

library(MPV)
housing <- table.b4
housing.model <- lm(y ~ ., data = housing)

# (a) Correlation matrix to detect multicollinearity
library("GGally")  # ggpairs
ggpairs(data = data.frame(housing[,-1]))
### We can see correlation between many regressors, including
### x1 vs. x2, x3, x4. This makes sense, given the meanings
### of each regressor (surface area is correlated to tax rates,
### number of bedrooms is correlated to number of rooms, etc).

# (b) Variance inflation factor and condition number
library(car)  # vif
vif(housing.model)  # cutoff value is 10 (MPV p. 118)

### Condition number, cutoff value is 100 (MPV p.298)
X <- model.matrix(housing.model)
XtX <- cor(X[,-1])
housing.eigen <- eigen(XtX)
max(housing.eigen$values) / min(housing.eigen$values)
### There is multicollinearity