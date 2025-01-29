# MPV Exercise 4.1

library(MPV)
### We consider the simple model y ~ x8 (opponents' rushing yards)
nfl <- table.b1
nfl.model <- lm(y ~ x8, data=nfl)
par(mfrow = c(1, 1))
plot(nfl$x8, nfl$y)
abline(nfl.model)

par(mfrow = c(2, 2))
plot(nfl.model)

# (a) Normal probability plot of residuals
par(mfrow = c(1, 1))
qqnorm(rstudent(nfl.model))  # rstudent = externally studentized
qqline(rstudent(nfl.model))
### No significant deviation

# (b) Residuals vs. fitted values
plot(nfl.model$fitted.values, nfl.model$residuals)
### Satisfactory distribution

# (c) Residuals vs. passing yardage (x2)
plot(nfl$x2, nfl.model$residuals)
### There appears to be a linear relationship, suggesting that we add x2
### to the model.


# MPV Exercise 4.2
### We consider the model y ~ x2 + x7 + x8
nfl.model <- lm(y ~ x2 + x7 + x8, data = table.b1)

# (a) Normal probability plot of residuals
qqnorm(rstudent(nfl.model))
qqline(rstudent(nfl.model))
### Nothing too abnormal - slightly light tails

# (b) Residuals vs. fitted values
plot(nfl.model$fitted.values, nfl.model$residuals)
### Satisfactory distribution

# (c) Residuals vs. regressor variables
par(mfrow = c(1, 3))
plot(nfl$x2, nfl.model$residuals)  # Slightly funnel shaped
plot(nfl$x7, nfl.model$residuals)  # Funnel shaped
plot(nfl$x8, nfl.model$residuals)
### No curved bands, so the regressor is well specified. The funnel shape
### indicates nonconstant variance.

# (d) Partial residual plots
library(car)  # avPlots
avPlots(nfl.model)
### We observe linear relationships between the y residuals and the x_i
### residuals, which confirms that the regression variables belong in the
### model.

# (e)
library(MASS)  # studres
par(mfrow = c(1, 2))
plot(rstandard(nfl.model), xlab="team", ylab="Studentized residual")
plot(rstudent(nfl.model), xlab="team", ylab="R-Student residual")
### With these plots we can identify influential points and potential
### outliers: for instance, the first point might be an outlier.