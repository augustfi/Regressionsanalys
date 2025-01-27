# MPV, Examples 2.1-2.2

### Import and view data
rocket <- read.table("rocket.data")
View(rocket)
par(mfrow = c(1, 1))
plot(rocket)

# 2.1: estimating beta_0 and beta_1
### By hand
S_xx <- sum((rocket$age - mean(rocket$age))^2)
S_xy <- sum(rocket$strength * (rocket$age - mean(rocket$age)))
beta_1 <- S_xy / S_xx
beta_0 <- mean(rocket$strength) - beta_1 * mean(rocket$age)
y_hat <- beta_0 + beta_1 * rocket$age
lines(rocket$age, y_hat)

### Using built-in functions
rocket.model <- lm(strength ~ age, data=rocket)
rocket.model$coefficients
plot(rocket)
lines(rocket$age, rocket.model$fitted.values)

# 2.2: estimate the variance
### By hand
SS_Res <- sum((rocket$strength - y_hat)^2)
n <- nrow(rocket)
sigma_hat2 <- MS_Res <- SS_Res / (n - 2)
sigma_hat <- sqrt(sigma_hat2)

### Using built-in functions
summary(rocket.model)$sigma

