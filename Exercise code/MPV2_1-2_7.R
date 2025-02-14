# MPV, Examples 2.1-2.7

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
summary(rocket.model)
summary(rocket.model)$coefficients[, "Estimate"]
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

# 2.3: t-test for null slope hypothesis
### By hand
t_0 <- beta_1 / sqrt(MS_Res / S_xx)
t_025 <- qt(.025, n - 2, lower.tail = FALSE)
x <- seq(from=-15, to=5, by=.1)
plot(x, dt(x, n - 2), type="l")
abline(v=-t_025)
abline(v=t_025)
abline(v=t_0)

### Using built-in functions
pt(t_0, n - 2, lower.tail = TRUE) * 2
summary(rocket.model)$coefficients["age",]

# 2.4: ANOVA
### By hand
SS_T <- sum((rocket$strength - mean(rocket$strength))^2)
SS_R <- SS_T - SS_Res
F_0 <- (SS_R / 1) / (SS_Res / (n - 2))
F_05 <- qf(.05, 1, n - 2)
x <- seq(from=0, to=200, by=1)
plot(x, df(x, 1, n - 2), type="l")
abline(v=F_05)
abline(v=F_0)

### Using built-in functions
summary(rocket.model)$fstatistic["value"]
pf(F_0, 1, n - 2, lower.tail = FALSE)

anova(rocket.model)

# 2.5: Confidence intervals for beta_0, beta_1, sigma2
### beta_0
beta_0.std <- sqrt(MS_Res * ((1 / n) + (mean(rocket$age)^2 / S_xx)))
beta_0.confint <- c(beta_0 - t_025 * beta_0.std, beta_0 + t_025 * beta_0.std)
names(beta_0.confint) <- c("2.5 %", "97.5 %")
beta_0.confint

confint(rocket.model, "(Intercept)", level=.95)

### beta_1
beta_1.std <- sqrt(MS_Res / S_xx)
beta_1.confint <- c(beta_1 - t_025 * beta_1.std, beta_1 + t_025 * beta_1.std)
names(beta_1.confint) <- c("2.5 %", "97.5 %")
beta_1.confint

confint(rocket.model, "age", level=.95)

### sigma2
chi2_025 <- qchisq(.025, n - 2, lower.tail = FALSE)
chi2_975 <- qchisq(.975, n - 2, lower.tail = FALSE)
sigma2.confint <- c((n - 2) * MS_Res / chi2_025, (n - 2) * MS_Res / chi2_975)
names(sigma2.confint) <- c("2.5 %", "97.5 %")
sigma2.confint

# 2.6: Confidence interval for mean response
x_0 <- data.frame(age = seq(from=min(rocket$age), to=max(rocket$age), by=1))
mean_resp.confint <- data.frame(predict(rocket.model, x_0, interval="confidence", level=.95))
plot(rocket)
lines(x_0$age, mean_resp.confint[,"fit"])
lines(x_0$age, mean_resp.confint[,"upr"])
lines(x_0$age, mean_resp.confint[,"lwr"])

# 2.7: Confidence interval for future observation
future_obs.confint <- data.frame(predict(rocket.model, x_0, interval="prediction", level=.95))
lines(x_0$age, future_obs.confint[,"upr"])
lines(x_0$age, future_obs.confint[,"lwr"])

