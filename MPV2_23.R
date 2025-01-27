# MPV, Exercise 2.23

m <- 500
n <- 20

coefs <- c()
mean_estimates <- c()
slope.confints <- c()
mean.confints <- c()

for (i in seq(m)) {
  x <- seq(.5, 10, .5)
  eps <- rnorm(length(x), mean = 0, sd = 4)
  y <- 50 + 10 * x + eps
  model <- lm(y ~ x)
  
  coefs <- rbind(coefs, model$coefficients)
  
  mean_estimates <- rbind(mean_estimates, predict(model, data.frame("x" = 5)))
  
  slope.confints <- rbind(slope.confints, confint(model)["x",])
  
  mean.confint <- predict(model, data.frame("x" = c(5)), interval = "confidence", evel = 0.95)
  mean.confints <- rbind(mean.confints, mean.confint)
}

#a
hist(coefs[, "(Intercept)"], 20)
hist(coefs[, "x"], 20)

#b
hist(mean_estimates, 20)

#c
sum((slope.confints[, "2.5 %"] < 10) & (slope.confints[, "97.5 %"] > 10)) / m

#d
sum((mean.confints[, "lwr"] < 100) & (mean.confints[, "upr"] > 100)) / m

