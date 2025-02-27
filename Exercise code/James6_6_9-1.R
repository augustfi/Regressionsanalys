# James et al., Exercise 6.6.9

library(ISLR)
data(College)

# (a) Split data into training and test sets
set.seed(669)
n <- nrow(College)
train <- sample(1:n, n / 2) # split in half
for (i in 2:ncol(College)) {  # normalize a bit?
  College[,i] <- scale(College[,i])
}
College.train <- College[train, ]
College.test <- College[-train, ]

# (b) Fit least squares linear model on training set
College.model <- lm(Apps ~ ., data = College.train)
### Report test error
model.pred <- predict(College.model, College.test)
model.MSE <- mean((College.test$Apps - model.pred) ^ 2)
model.MSE
cat('Test error of the linear model:', model.MSE)

# (c) Ridge regression
library(glmnet)  # elastic net
x.train <- data.matrix(College.train[,-2])
y.train <- data.matrix(College.train[,2])
College.ridge <- cv.glmnet(as.matrix(x.train), as.numeric(y.train), alpha=0, lambda = seq(.001,10,.005))  # alpha = 0 means ridge regression
plot(College.ridge)
College.ridge$lambda.1se  # one standard deviation away from minimizing lambda
coef(College.ridge)  # coefficients for lambda.1se

### Report test error
x.test <- data.matrix(College.test[,-2])
ridge.pred <- predict(College.ridge, s = College.ridge$lambda.1se, newx = x.test)
ridge.MSE <- mean((College.test$Apps - ridge.pred)^2)
ridge.MSE
cat('Test error of the ridge regression model:', ridge.MSE)

### Plot ridge traces from cv.glmnet
X.matrix <- College.train[,-2]
glmnet.fit <- College.ridge$glmnet.fit
default.colors <- seq_len(ncol(X.matrix))
matplot(glmnet.fit$lambda, t(glmnet.fit$beta), type="l", xlab = "lambda", ylab = "coefficients", col=default.colors)
legend("topright", colnames(X.matrix), col=default.colors,cex=0.8,fill=default.colors)

# (d) Lasso
College.lasso <- cv.glmnet(x.train, y.train, alpha=1)  # alpha = 1 means lasso
plot(College.lasso)
College.lasso$lambda.1se

### Report test error
lasso.pred <- predict(College.lasso, s = College.lasso$lambda.1se, newx = x.test)
lasso.MSE <- mean((College.test$Apps - lasso.pred)^2)
lasso.MSE
cat('Test error of the lasso regression model:', lasso.MSE)

### Report coefficient estimates
coefs.lasso <- coef(College.lasso)  # coefficients for lambda.1se
coefs.lasso[rowSums(coefs.lasso) != 0,]  # nonzero coefficients

# (e) Principal component regression
library(pls)  # pcr
College.pcr <- pcr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
coef(College.pcr)
summary(College.pcr)
validationplot(College.pcr, val.type = "MSEP")   # MSE vs. number of components
### Stabilization at m = 5
### Report test error
pcr.pred <- predict(College.pcr, x.test, ncomp = 5)
pcr.MSE <- mean((College.test$Apps - pcr.pred[,,1])^2)
pcr.MSE
cat('Test error of the PCR model:', pcr.MSE)

# (g) Compare the methods
model.MSE
ridge.MSE
lasso.MSE
pcr.MSE
### The ridge regression model minimizes the MSE by definition. Ridge and lasso
### have comparable predicting power, with PCR falling behind.
### The power of least square and ridge is very close, may depend on the data and random set.
