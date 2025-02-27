# James et al., Exercise 6.6.8

# (a) Generate regressor and noise
set.seed(668)
n <- 100
x <- rnorm(n)
epsilon <- rnorm(n)

# (b) Generate response
beta <- runif(4, min = -10, max = 10) # beta0 to beta3 (OBS! The indices are 1 to 4)
X <- cbind(1, x, x^2, x^3)  # regressor variable matrix
y <- X %*% beta + epsilon
plot(x, y)

# (c) Full subset selection
library(leaps)  # regsubsets
X.full <- c()
for (k in 1:10) {
  X.full <- cbind(X.full, x^k)
  colnames(X.full)[k] <- paste("x^", toString(k))
}
full.model <- regsubsets(X.full, y, nvmax = 10)
full.summary <- summary(full.model)
plot(full.model, scale="Cp")
plot(full.model, scale="bic")
plot(full.model, scale="adjr2")
which.min(full.summary$cp)
which.min(full.summary$bic)
which.max(full.summary$adjr2)

### Plot criteria vs. number of regressors
n.reg <- c(1:10)
plot(n.reg, full.summary$cp, type="l")
plot(n.reg, full.summary$bic, type="l")
plot(n.reg, full.summary$adjr2, type="l")

coef(full.model, 5) # Extract coefficients for the 3-variable model

# (d) Forward and backward stepwise selection
### Forward
forward.model <- regsubsets(X.full, y, nvmax = 10, method="forward")
forward.summary <- summary(forward.model)
which.min(forward.summary$cp)
which.min(forward.summary$bic)
which.max(forward.summary$adjr2)
plot(forward.model, scale="Cp")
plot(forward.model, scale="bic")
plot(forward.model, scale="adjr2")

coef(forward.model, 4)

### Backward
backward.model <- regsubsets(X.full, y, nvmax = 10, method="backward")
backward.summary <- summary(backward.model)
which.min(backward.summary$cp)
which.min(backward.summary$bic)
which.max(backward.summary$adjr2)
plot(backward.model, scale="Cp")
plot(backward.model, scale="bic")
plot(backward.model, scale="adjr2")

coef(backward.model, 3)

# (e) Lasso model
library(glmnet)
X.lasso <- cv.glmnet(X.full, y, alpha=1)  # lasso
X.lasso$lambda.1se
plot(X.lasso)

coef(X.lasso)[rowSums(coef(X.lasso)) != 0,]  # nonzero coefficients
