# Exercise MPV 5.10
# Consider the presssure drop data in table b.9

# (a) Thorough residual analysis
library(MPV)
pressure <- table.b9  # overwrites a base dataset, apparently

# Take the full model: all regression variables
pressure.model <- lm(y ~ x1 + x2 + x3 + x4, data=pressure)

# Normal probability plot of residuals
par(mfrow = c(1, 1))
qqnorm(rstudent(pressure.model))
qqline(rstudent(pressure.model))
### Good normal probability plot

# Residuals vs. fitted values
plot(pressure.model$fitted.values, pressure.model$residuals)
### Horizontal band, so satisfactory distribution

# Residuals vs. regressor variables
par(mfrow = c(2, 2))
plot(pressure$x1, pressure.model$residuals)
plot(pressure$x2, pressure.model$residuals)
plot(pressure$x3, pressure.model$residuals)
plot(pressure$x4, pressure.model$residuals)
### We can't say much, except for a curved band for x4

# Partial residual plots
library(car)  # avPlots, boxcox, boxTidwell
avPlots(pressure.model)
### Somewhat linear relationships for x1, x2, x3. Clearly curved for
### x4.

# Studentized residuals
library(MASS)  # studres
par(mfrow = c(1, 2))
plot(studres(pressure.model))
plot(rstudent(pressure.model))

# (b) Transform data
par(mfrow = c(1, 1))
boxcox(pressure.model)
boxTidwell(y ~ x1 + x2 + x3 + x4, data=pressure) # rather unreliable
boxTidwell(y ~ x1 + x2 + x3, ~ x4 + I(x4^2), data=pressure)

pressure.model2 <- lm(y ~ x2 + x3 + x4 + I(x4^2), data=pressure)
summary(pressure.model2)
par(mfrow = c(2, 2))
plot(pressure.model2)

par(mfrow = c(1, 1))
avPlots(pressure.model2)
### Adding x4^2 improves the partial residual plots